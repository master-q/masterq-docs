#!/bin/sh

TOPDIR="/home/kiwamu/wiki/gitit"
NAME=gitit
PIDFILE=$TOPDIR/gitit_daemon.pid
PROG="/home/kiwamu/.cabal/bin/gitit"
OPTION="-f gitit_daemon.conf"
SSD=/sbin/start-stop-daemon

start() {
	echo -n "Starting $NAME: "
	$SSD --start --pidfile $PIDFILE --make-pidfile --background \
	  --user kiwamu --chdir $TOPDIR --exec $PROG -- $OPTION
	RETVAL=$?
	echo
	return $RETVAL
}

stop() {
	echo -n "Stopping $NAME: "
	$SSD --stop --oknodo --pidfile $PIDFILE
	RETVAL=$?
	echo
	return $RETVAL

}

restart() {
	stop
	start
}

# main
case "$1" in
	start)
		start
		;;
	stop)
		stop
		;;
	restart)
		restart
		;;
	*)
		echo "Usage: $0 {start|stop|restart}"
		exit 1
esac

