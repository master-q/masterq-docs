# Play, Slick, play2-authの間で討死

![background](img/Forty-seven-Ronin.png)

Kiwamu Okabe

# あんた誰？

![background](img/enjoy.png)

* http://www.masterq.net/
* 名前: 岡部 究 (オカベ キワム)
* 元組み込みエンジニア
* Web関連技術初心者
* Scalaは1ヶ月使って挫折
* 今日はその挫折の話をしようと思います

# Playを使ってみようという話に

* 某案件でWebフレームワークを選定
* なにはともあれ型の強い言語を使いたい
* まずはScala+Playをさわってみよう

![inline](img/play.png)

# どーせならSlick使おう

* 次期PlayではSlickが標準サポートに
* それ以上Slickについて良く知らなかった

![inline](img/slick.png)

# 認証はplay2-authを

* ユーザ/パスワード認証が欲しいだけだった
* SecureSocialはごっつい
* play2-authはマニュアルがわかりやすい

![inline](img/t2v.png)

# build.sbt

```
libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "mysql" % "mysql-connector-java"    % "5.1.34",
  "com.typesafe.play" %% "play-slick" % "0.8.1",
  "jp.t2v" %% "play2-auth"            % "0.13.0",
  "jp.t2v" %% "play2-auth-test"       % "0.13.0" % "test",
  "jp.t2v" %% "stackable-controller"  % "0.4.1"
)
```

# conf/routes

```
# Home page
GET     /          controllers.Application.login
POST    /login     controllers.Application.authenticate
GET     /logout    controllers.Application.logout
```

# conf/application.conf

```
db.default.driver=org.h2.Driver
db.default.url="jdbc:h2:mem:play"
db.default.user=sa
db.default.password=""

slick.default="models.*"
```

# app/models/Account.scala #1

```scala
package models

import play.api.db.slick.Config.driver.simple._

case class Account(station_id: String, member_id: String, password: String)

class Accounts(tag: Tag) extends Table[Account](tag, "ACCOUNT") {
  def station_id = column[String]("STATION_ID")
  def member_id = column[String]("MEMBER_ID")
  def password = column[String]("PASSWORD")
  def * = (station_id, member_id, password) <> (Account.tupled, Account.unapply _)
  def pk = primaryKey("pk_a", (station_id, member_id))
}
```

# app/models/Account.scala #2

```scala
object Accounts {
  val account = TableQuery[Accounts]

  def authenticate(sid: String, mid: String, password: String): Option[Account] = {
    findById((sid, mid)).filter {
      account => password.equals(account.password)
    }
  }

  // xxx
  def findById(smid: (String, String)): Option[Account] = None
  // xxx
  def findAll(): Seq[Account] = Seq.empty
  // xxx
  def create(account: Account) {
  }
}
```

# app/models/Role.scala

```scala
package models

sealed trait Role
case object Administrator extends Role
case object NormalUser extends Role

object Role {
  def valueOf(value: String): Role = value match {
    case "Administrator" => Administrator
    case "NormalUser"    => NormalUser
    case _ => throw new IllegalArgumentException()
  }
}
```

# app/Global.scala

```scala
import play.api._
import models._

object Global extends GlobalSettings {
  override def onStart(app: Application) {
    if (Accounts.findAll.isEmpty) {
      Seq(
        Account("100001", "100001", "1")
      ) foreach Accounts.create
    }
  }
}
```

# Application.scala #1

app/controllers/Application.scala

```scala
package controllers

import jp.t2v.lab.play2.auth._
import models._
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import play.api.mvc.Results._

import scala.concurrent.{Future, ExecutionContext}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.reflect._
```

# Application.scala #2

```scala
trait AuthConfigImpl extends AuthConfig {
  type Id = (String, String)
  type User = Account
  val idTag: ClassTag[Id] = classTag[Id]
  val sessionTimeoutInSeconds: Int = 3600

  def resolveUser(id: Id)(implicit ctx: ExecutionContext): Future[Option[User]] = Future.successful(Accounts.findById(id))
  def loginSucceeded(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Redirect(routes.Application.login))
  def logoutSucceeded(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Redirect(routes.Application.login))
  def authenticationFailed(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Redirect(routes.Application.login))
  def authorizationFailed(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Forbidden("no permission"))
  def authorize(user: User, authority: Authority)(implicit ctx: ExecutionContext): Future[Boolean] =
    Future.successful(false) // xxx
}
```

# Application.scala #3

```scala
object Application extends Controller with LoginLogout with AuthConfigImpl {
  val loginForm = Form {
    mapping("station_id" -> text, "member_id" -> text, "password" -> text)(Accounts.authenticate)(_.map(u => (u.station_id, u.member_id, "")))
      .verifying("Invalid email or password", result => result.isDefined)
  }
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm))
  }
  def logout = Action.async { implicit request =>
    gotoLogoutSucceeded.map(_.flashing(
      "success" -> "You've been logged out"
    ))
  }
  def authenticate = Action.async { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(views.html.login(formWithErrors))),
      user           => gotoLoginSucceeded((user.get.station_id, user.get.member_id))
    )
  }
}
```

# でもこれ認証してなくね？

# 振り返り

* みんな認証まわりはどーしてるんだろう？
* 実はSlick使わない方が楽？
