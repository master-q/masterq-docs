# ocamloptの全体像

Kiwamu Okabe

# 私は誰？
![background](img/enjoy.png)

* Twitter: @master_q
* Metasepiプロジェクト主催
* Ajhc Haskellコンパイラ開発者
* Debian Maintainer
* 前はデジタルサイネージの開発してました
* その昔はNetBSDでコピー機作ってた

# 今日のもくじ

xxx

# まずはOCaml 4.00.1をインストール

あ、Debian sid前提です...

~~~
$ git clone git@github.com:ocaml/ocaml.git
$ cd ocaml
$ git checkout 4.00.1
$ ./configure -with-debug-runtime
$ make world.opt
$ sudo make install
$ which ocamlopt
/usr/local/bin/ocamlopt
$ ocamlopt -version
4.00.1
~~~

# ocamloptってなんどすか？

OCamlソースを実行バイナリにするコンパイラ

~~~
$ cat hello.ml
let hello _ = print_endline "Hello world!";;
let _ = hello ();
$ ocamlopt -g -runtime-variant d -o hello.bin hello.ml
$ file hello.bin
hello.bin: ELF 64-bit LSB  executable, x86-64, version 1 (SYSV), dynamically linked (uses shared libs), for GNU/Linux 2.6.32, BuildID[sha1]=402030a9bb82606a9d38f73e6ec25455f96d3caf, not stripped
$ ./hello.bin
### OCaml runtime: debug mode ###
Initial minor heap size: 2048k bytes
Initial major heap size: 992k bytes
Initial space overhead: 80%
Initial max overhead: 500%
Initial heap increment: 992k bytes
Initial allocation policy: 0
Hello world!
~~~

# -with-debug-runtime？ -g？

* objdump -S hello.bin してみると...

![inline](img/objdump.png)

# どーいうことだってばよ!？

* objdumpやgdbで
* 実行バイナリの中の
* コンパイル結果マシン語と
* ランタイムのマシン語の両方を
* ソースコードレベルデバッグできる

すばらしい!

# OCamlプログラムの起動

* main関数から実行が開始されて
* camlHello__entry関数がプログラム入口

かな？

~~~
$ ocamlopt -o hello.bin hello.ml
$ gdb hello.bin
(gdb) b camlHello__entry 
Breakpoint 1 at 0x402260
(gdb) run
...
Breakpoint 1, 0x0000000000402260 in camlHello__entry ()
(gdb) bt
#0  0x0000000000402260 in camlHello__entry ()
#1  0x0000000000401e89 in caml_program ()
#2  0x0000000000410d82 in caml_start_program ()
#3  0x0000000000411245 in caml_main ()
#4  0x000000000040414e in main ()
~~~

# main関数はどこに？

* "byterun/main.c"です
* え？バイトコード？
* OCamlには"{byte,asm}run"のディレクトリ
* でもなんかきっちり分離されていない
* バイナリでも適宜byterunの下を使います

# main関数ソース

デバッグ機能なしだとこんなん

~~~ {.c}
/* File: byterun/main.c */
int main(int argc, char **argv)
{
  caml_main(argv);
  caml_sys_exit(Val_int(0));
  return 0; /* not reached */
}
~~~

むちゃくちゃ簡単です

# caml_main関数前半

まー最初は初期化です

~~~ {.c}
/* File: asmrun/startup.c caml_main関数前半 */
void caml_main(char **argv)
{
/* --snip-- */
  caml_init_ieee_floats();
  caml_init_custom_operations();
  caml_top_of_stack = &tos;
  parse_camlrunparam();
  caml_init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
                percent_free_init, max_percent_free_init);
  init_atoms();
  caml_init_signals();
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = "";
  exe_name = caml_search_exe_in_path(exe_name);
  caml_sys_init(exe_name, argv);
~~~

# caml_init_ieee_floats関数

~~~ {.c}
/* File: byterun/floats.c */
/* The [caml_init_ieee_float] function should initialize
   floating-point hardware so that it behaves as much as possible
   like the IEEE standard.
   In particular, return special numbers like Infinity and NaN
   instead of signalling exceptions.  Currently, everyone is in
   IEEE mode at program startup, except FreeBSD prior to 4.0R. */
/* --snip-- */
void caml_init_ieee_floats(void)
{
#if defined(__FreeBSD__) && (__FreeBSD_version < 400017)
  fpsetmask(0);
#endif
}
~~~

これは...FreeBSDへのバグ対策では...

# caml_init_custom_operations #1

ボックス化されたintのプリミティブ関数を登録

~~~ {.c}
/* File: byterun/custom.c */
void caml_init_custom_operations(void)
{
  caml_register_custom_operations(&caml_int32_ops);
  caml_register_custom_operations(&caml_nativeint_ops);
  caml_register_custom_operations(&caml_int64_ops);
}
~~~

# caml_init_custom_operations #2

でもたぶんこれバイトコードでだけ必要なんだと思う

~~~
$ grep "caml_.*_ops" asmcomp/cmmgen.ml
    Pnativeint -> "caml_nativeint_ops"
  | Pint32 -> "caml_int32_ops"
  | Pint64 -> "caml_int64_ops"
    Csymbol_address("caml_int32_ops") :: Cint32 n :: Cint32 0n :: cont
    Csymbol_address("caml_int32_ops") :: Cint n :: cont
  Csymbol_address("caml_nativeint_ops") :: Cint n :: cont
    Csymbol_address("caml_int64_ops") :: Cint lo :: cont
      Csymbol_address("caml_int64_ops") :: Cint hi :: Cint lo :: cont
      Csymbol_address("caml_int64_ops") :: Cint lo :: Cint hi :: cont
~~~

コンパイラパイプラインが勝手にシンボルからアドレス引く

# parse_camlrunparam関数

OCAMLRUNPARAM環境変数を見る、まぁddy

~~~
http://caml.inria.fr/pub/docs/manual-ocaml/manual024.html
~~~

~~~ {.c}
/* File: asmrun/startup.c */
static void parse_camlrunparam(void)
{
  char *opt = getenv ("OCAMLRUNPARAM");
  uintnat p;

  if (opt == NULL) opt = getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &caml_verb_gc); break;
      case 'b': caml_record_backtrace(Val_true); break;
      case 'p': caml_parser_trace = 1; break;
      case 'a': scanmult (opt, &p); caml_set_allocation_policy (p); break;
      }
    }
  }
}
~~~

# caml_init_gc関数

メジャー/マイナーGC用のヒープ初期化

Real World OCaml読んだ方がいいかも

~~~ {.c}
/* File: byterun/gc_ctrl.c */
void caml_init_gc (uintnat minor_size, uintnat major_size,
                   uintnat major_incr, uintnat percent_fr,
                   uintnat percent_m)
{
  uintnat major_heap_size = Bsize_wsize (norm_heapincr (major_size));

  if (caml_page_table_initialize(Bsize_wsize(minor_size) + major_heap_size)){
    caml_fatal_error ("OCaml runtime error: cannot initialize page table\n");
  }
  caml_set_minor_heap_size (Bsize_wsize (norm_minsize (minor_size)));
  caml_major_heap_increment = Bsize_wsize (norm_heapincr (major_incr));
  caml_percent_free = norm_pfree (percent_fr);
  caml_percent_max = norm_pmax (percent_m);
  caml_init_major_heap (major_heap_size);
}
~~~

# init_atoms関数

xxx caml_data_segmentsとcaml_code_segmentsを解析しているように見える

~~~ {.c}
/* File: asmrun/startup.c */

/* Initialize the atom table and the static data and code area limits. */
~~~

# caml_init_signals関数

SIGSEGVのシグナルハンドラを通常コンテキストとは別のスタックで実行するように設定

~~~ {.c}
/* File: asmrun/signals_asm.c */
void caml_init_signals(void)
{
  /* Stack overflow handling */
    stack_t stk;
    struct sigaction act;
    stk.ss_sp = sig_alt_stack;
    stk.ss_size = SIGSTKSZ;
    stk.ss_flags = 0;
    SET_SIGACT(act, segv_handler);
    act.sa_flags |= SA_ONSTACK | SA_NODEFER;
    sigemptyset(&act.sa_mask);
    system_stack_top = (char *) &act;
    if (sigaltstack(&stk, NULL) == 0) { sigaction(SIGSEGV, &act, NULL); }
}
~~~

# segv_handler関数

~~~ {.c}
/* File: asmrun/signals_osdep.h */
#define CONTEXT_PC (context->uc_mcontext.gregs[REG_RIP])
#define CONTEXT_EXCEPTION_POINTER (context->uc_mcontext.gregs[REG_R14])
#define CONTEXT_YOUNG_PTR (context->uc_mcontext.gregs[REG_R15])
#define CONTEXT_FAULTING_ADDRESS ((char *) context->uc_mcontext.gregs[REG_CR2])

/* File: asmrun/signals_asm.c */
DECLARE_SIGNAL_HANDLER(segv_handler)
{
  fault_addr = CONTEXT_FAULTING_ADDRESS;
  if (((uintnat) fault_addr & (sizeof(intnat) - 1)) == 0
      && getrlimit(RLIMIT_STACK, &limit) == 0
      && fault_addr < system_stack_top
      && fault_addr >= system_stack_top - limit.rlim_cur - EXTRA_STACK
      && Is_in_code_area(CONTEXT_PC)
      ) {
    /* Turn this into a Stack_overflow exception */
    caml_exception_pointer = (char *) CONTEXT_EXCEPTION_POINTER;
    caml_young_ptr = (char *) CONTEXT_YOUNG_PTR;
    caml_raise_stack_overflow();
  }
  /* Otherwise, deactivate our exception handler and return,
     causing fatal signal to be generated at point of error. */
  act.sa_handler = SIG_DFL;
  act.sa_flags = 0;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, NULL);
}
~~~

# caml_raise_stack_overflow関数

~~~ {.c}
/* File: asmrun/fail.c */
void caml_raise(value v)
{
  Unlock_exn();
  if (caml_exception_pointer == NULL) caml_fatal_uncaught_exception(v);

#define PUSHED_AFTER <
  while (caml_local_roots != NULL &&
         (char *) caml_local_roots PUSHED_AFTER caml_exception_pointer) {
    caml_local_roots = caml_local_roots->next;
  }

  caml_raise_exception(v);
}

void caml_raise_stack_overflow(void)
{
  caml_raise((value) &caml_bucket_Stack_overflow);
}
~~~

# caml_raise_exception

~~~ {.gnuassembler}
#define C_ARG_1 %rdi
#define TESTL_VAR(imm,label) \
        testl   imm, G(label)(%rip)
#define LOAD_VAR(srclabel,dstreg) \
        movq    G(srclabel)(%rip), dstreg

FUNCTION(G(caml_raise_exception))
        TESTL_VAR($1, caml_backtrace_active)
        jne     LBL(111)
        movq    C_ARG_1, %rax
        LOAD_VAR(caml_exception_pointer, %rsp)  /* Cut stack */
        popq    %r14                  /* Recover previous exception handler */
        LOAD_VAR(caml_young_ptr, %r15) /* Reload alloc ptr */
        ret
~~~

# 特別扱いするレジスタ

~~~ {.ocaml}
(* File: asmcomp/amd64/emit.mlp *)
let emit_instr fallthrough i =
    emit_debug_info i.dbg;
    match i.desc with
(* --snip-- *)
    | Lpushtrap ->
        cfi_adjust_cfa_offset 8;
        `	pushq	%r14\n`;
        cfi_adjust_cfa_offset 8;
        `	movq	%rsp, %r14\n`;
        stack_offset := !stack_offset + 16
    | Lpoptrap ->
        `	popq	%r14\n`;
        cfi_adjust_cfa_offset (-8);
        `	addq	$8, %rsp\n`;
        cfi_adjust_cfa_offset (-8);
        stack_offset := !stack_offset - 16
    | Lraise ->
        if !Clflags.debug then begin
          `	{emit_call "caml_raise_exn"}\n`;
          record_frame Reg.Set.empty i.dbg
        end else begin
          `	movq	%r14, %rsp\n`;
          `	popq	%r14\n`;
          `	ret\n`
        end
~~~

# caml_debugger_init関数

CAML_DEBUG_SOCKET環境変数が定義されていたらBSDソケットを使ったデバッグ環境を設定

~~~
http://caml.inria.fr/pub/docs/manual-ocaml/manual030.html
~~~

# caml_sys_init関数

プログラム名と引数の保管

~~~ {.c}
/* File: byterun/sys.c */
void caml_sys_init(char * exe_name, char **argv)
{
  caml_exe_name = exe_name;
  caml_main_argv = argv;
}
~~~

# caml_main関数後半

~~~ {.c}
/* File: asmrun/startup.c caml_main関数後半 */
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return;
  }
  res = caml_start_program();
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}
~~~

# longjmpするのは誰？

スレッドが終了するとlongjmpする。
mainスレッドじゃなければ、caml_termination_jmpbufには飛ばない。

~~~ {.c}
/* File: otherlibs/systhreads/st_stubs.c */
CAMLprim value caml_thread_initialize(value unit)   /* ML */
{
/* --snip-- */
  curr_thread->exit_buf = &caml_termination_jmpbuf;

/* --snip-- */
static ST_THREAD_FUNCTION caml_thread_start(void * arg)
{
/* --snip-- */
  if (sigsetjmp(termination_buf.buf, 0) == 0) {
    th->exit_buf = &termination_buf;

/* --snip-- */
CAMLprim value caml_thread_exit(value unit)   /* ML */
{
/* --snip-- */
  exit_buf = curr_thread->exit_buf;
  caml_thread_stop();
  if (exit_buf != NULL) {
    siglongjmp(exit_buf->buf, 1);
~~~

# caml_start_program

~~~ {.gnuassembler}
/* File: asmrun/amd64.S */
FUNCTION(G(caml_start_program))
        CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
        CFI_ADJUST(56)
    /* Initial entry point is G(caml_program) */
        leaq    GCALL(caml_program)(%rip), %r12
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Build a callback link */
        subq    $8, %rsp        /* stack 16-aligned */
        PUSH_VAR(caml_gc_regs)
        PUSH_VAR(caml_last_return_address)
        PUSH_VAR(caml_bottom_of_stack)
        CFI_ADJUST(32)
    /* Setup alloc ptr and exception ptr */
        LOAD_VAR(caml_young_ptr, %r15)
        LOAD_VAR(caml_exception_pointer, %r14)
    /* Build an exception handler */
        lea     LBL(108)(%rip), %r13
        pushq   %r13
        pushq   %r14
        CFI_ADJUST(16)
        movq    %rsp, %r14
    /* Call the OCaml code */
        call    *%r12
~~~

# caml_program

~~~ {.gnuassembler}
/* File: hello.bin.startup.s (コンパイル時生成) */
caml_program:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset	8
	call	camlPervasives__entry@PLT
	movq	caml_globals_inited@GOTPCREL(%rip), %rax
	addq	$1, (%rax)
	call	camlHello__entry@PLT
~~~

# camlPervasives__entry

~~~ {.ocaml}
(* File: stdlib/pervasives.ml *)
external flush : out_channel -> unit = "caml_ml_flush"
external out_channels_list : unit -> out_channel list
                           = "caml_ml_out_channels_list"
let flush_all () =
  let rec iter = function
      [] -> ()
    | a :: l -> (try flush a with _ -> ()); iter l
  in iter (out_channels_list ())
let exit_function = ref flush_all
let do_at_exit () = (!exit_function) ()
external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"
let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
~~~

# caml_register_named_value

~~~ {.c}
/* File: byterun/callback.c */
CAMLprim value caml_register_named_value(value vname, value val)
{
  struct named_value * nv;
  char * name = String_val(vname);
  unsigned int h = hash_value_name(name);

  for (nv = named_value_table[h]; nv != NULL; nv = nv->next) {
    if (strcmp(name, nv->name) == 0) {
      nv->val = val;
      return Val_unit;
    }
  }
  nv = (struct named_value *)
         caml_stat_alloc(sizeof(struct named_value) + strlen(name));
  strcpy(nv->name, name);
  nv->val = val;
  nv->next = named_value_table[h];
  named_value_table[h] = nv;
  caml_register_global_root(&nv->val);
  return Val_unit;
}
~~~

# caml_register_global_root

flush処理を登録する

xxx 誰が実行するの？

~~~ {.c}
/* File: byterun/globroots.c */
CAMLexport void caml_register_global_root(value *r)
{
  Assert (((intnat) r & 3) == 0);  /* compact.c demands this (for now) */
  caml_insert_global_root(&caml_global_roots, r);
}
~~~

# camlHello__entry

caml_globals_inited++ してから呼ばれる

ゴール!

# 起動の地図にまとめましょう

# じゃーどうやって文字列を出力しているの？

