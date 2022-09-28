package core

import java.io.PrintStream

enum Logging:
  case On
  case Off

// Logger with support for nested contexts.
// https://gist.github.com/loverdos/25079d10636446f311adc0a57110d609
final class Logger(
  out: PrintStream = System.out,
  logging: Logging = Logging.On,
  indentation_cutoff: Int = 20,
):
  private var _logging = logging
  private var ctx = List[String]()
  private var logs_in_context = 0 :: Nil

  private val indent_chunk = "  "

  private def ctx_size = ctx.length

  def get_logging(): Logging = _logging
  def is_logging(logging: Logging): Boolean = _logging == logging
  def is_logging_on: Boolean = is_logging(Logging.On)
  def is_logging_off: Boolean = is_logging(Logging.Off)
  def set_logging(logging: Logging): this.type =
    _logging = logging
    this
  end set_logging
  def set_logging_on(): this.type = set_logging(Logging.On)
  def set_logging_off(): this.type = set_logging(Logging.Off)

  private def get_log_prefix(
    _indent: Int,
    is_context: Boolean,
    is_context_push: Boolean
  ): String =
    // too much indentation (especially when debugging nested call stacks) is visually useless,
    // so we introduce a cut-off.
    val indent = Math.min(_indent, indentation_cutoff)
    val sb = new StringBuilder

    val indent_chunk_size: Int = indent_chunk.length
    val max_n = indent * indent_chunk_size
    for n <- 0 to max_n do
      val s: String =
        (n, is_context, is_context_push) match
          case (n, true, true) if n == max_n => ">"
          case (n, true, true) if (n % indent_chunk_size) == 0 => "+"
          case (_, true, true) => "-"
          case (n, true, false) if n == max_n => "<"
          case (n, true, false) if (n % indent_chunk_size) == 0 => "+"
          case (_, true, false) => "-"
          case (n, false, _) if (n % indent_chunk_size) == 0 => "|"
          case (_, false, _)  => " "
        end match

      sb.append(s)
    end for

    val indent_diff = _indent - indent
    if indent_diff > 0 then
      sb.append(s" [${indent_diff}] ")
    end if

    sb.toString()
  end get_log_prefix


  private def println_raw(s: String): Unit =
    if is_logging_on then
      out.println(s)
      out.flush()
    end if
  end println_raw

  def push_ctx(name: String, info: String = ""): Unit =
    ctx = name :: ctx
    logs_in_context = 0 :: logs_in_context

    val prefix = get_log_prefix(ctx_size, true, true)
    //val msg = prefix + " (" + name + ")"
    val msg = prefix + " " + name
    println_raw(msg)
    if info.nonEmpty then
      log(">> " + info)
    end if
  end push_ctx

  def pop_ctx(info: String = ""): Unit =
    val howmany_logs = logs_in_context.head

    // log the end of context only if something has been logged in the context.
    // This is to avoid some verbosity and, so, aid in debugging.
    if howmany_logs > 0 then
      val name = ctx.head
      val prefix = get_log_prefix(ctx_size, true, false)
      //val msg = prefix + " (" + name + ")"
      val msg = prefix + " " + name
      if info.nonEmpty then
        log("<< " + info)
      end if
      println_raw(msg)
    end if

    ctx = ctx.tail
    logs_in_context = logs_in_context.tail
  end pop_ctx

  def log(ss: String*): Unit =
    val prefix = get_log_prefix(ctx_size, false, false)
    val s = ss.mkString("")

    println_raw(prefix + " " + s)

    logs_in_context = (logs_in_context.head + 1) :: logs_in_context.tail
  end log
end Logger
