package trees

import java.io.{PrintWriter, StringWriter}
import core.*

enum Indentation:
  case Same
  case More
  case Less
end Indentation

// Utility class to aid in writing source code.
class SourceCodeWriter(initial_indent_level: Int = 0):
  protected val sw = new StringWriter(1024)
  protected val pw = new PrintWriter(sw)
  protected val indent_str = "  "
  protected var indent_level = initial_indent_level
  protected var current_line_width = 0

  def inc_indent()       = indent_level += 1
  def dec_indent()       = indent_level -= 1
  def get_indent_str()   = indent_str * indent_level
  def get_indent_level() = indent_level

  def get_code(): String =
    pw.flush()
    sw.toString
  end get_code

  def has_code(): Bool = sw.getBuffer.length() > 0

  def write_code(s: String): this.type =
    if current_line_width == 0 then
      val indent_str = get_indent_str()
      write_raw_str(indent_str)
    end if

    write_raw_str(s)

    this
  end write_code

  def write_raw_str(s: String): this.type =
    pw.print(s)
    current_line_width += s.length
    this
  end write_raw_str

  def write_code_ln(s: String): this.type =
    write_code(s)
    write_ln()
  end write_code_ln

  def write_ln(): this.type =
    pw.println()
    current_line_width = 0

    this
  end write_ln

  def with_indentation[C](indentation: Indentation)(closure: => C): C =
    do_indentation(indentation)
    val result = closure
    do_reverse_indentation(indentation)
    result
  end with_indentation

  private def do_indentation(indentation: Indentation): this.type =
    indentation match
      case Indentation.Same =>
      case Indentation.More => inc_indent()
      case Indentation.Less => dec_indent()
    end match

    this
  end do_indentation

  private def do_reverse_indentation(indentation: Indentation): this.type =
    indentation match
      case Indentation.Same =>
      case Indentation.More => dec_indent()
      case Indentation.Less => inc_indent()
    end match

    this
  end do_reverse_indentation

  def new_with_same_indentation(): SourceCodeWriter = new SourceCodeWriter(indent_level)

  def new_with_indentation(indentation: Indentation): SourceCodeWriter =
    new SourceCodeWriter(indent_level).do_indentation(indentation)
  end new_with_indentation
end SourceCodeWriter
