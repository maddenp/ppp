# Autogenerated from a Treetop grammar. Edits may be lost.


require "normfree_parser"

module Normfree
  include Treetop::Runtime

  def root
    @root ||= :text
  end

  def _nt_text
    start_index = index
    if node_cache[:text].has_key?(index)
      cached = node_cache[:text][index]
      if cached
        cached = SyntaxNode.new(input, index...(index + 1)) if cached == true
        @index = cached.interval.end
      end
      return cached
    end

    s0, i0 = [], index
    loop do
      i1 = index
      r2 = _nt_sms
      if r2
        r1 = r2
      else
        r3 = _nt_directive
        if r3
          r1 = r3
        else
          r4 = _nt_comment
          if r4
            r1 = r4
          else
            r5 = _nt_quoted
            if r5
              r1 = r5
            else
              r6 = _nt_unquoted
              if r6
                r1 = r6
              else
                @index = i1
                r1 = nil
              end
            end
          end
        end
      end
      if r1
        s0 << r1
      else
        break
      end
    end
    r0 = instantiate_node(Text,input, i0...index, s0)

    node_cache[:text][start_index] = r0

    r0
  end

  module Sms0
  end

  def _nt_sms
    start_index = index
    if node_cache[:sms].has_key?(index)
      cached = node_cache[:sms][index]
      if cached
        cached = SyntaxNode.new(input, index...(index + 1)) if cached == true
        @index = cached.interval.end
      end
      return cached
    end

    i0, s0 = index, []
    if has_terminal?("@", false, index)
      r1 = instantiate_node(SyntaxNode,input, index...(index + 1))
      @index += 1
    else
      terminal_parse_failure("@")
      r1 = nil
    end
    s0 << r1
    if r1
      if has_terminal?('\G[sS]', true, index)
        r2 = true
        @index += 1
      else
        r2 = nil
      end
      s0 << r2
      if r2
        if has_terminal?('\G[mM]', true, index)
          r3 = true
          @index += 1
        else
          r3 = nil
        end
        s0 << r3
        if r3
          if has_terminal?('\G[sS]', true, index)
            r4 = true
            @index += 1
          else
            r4 = nil
          end
          s0 << r4
          if r4
            if has_terminal?("$", false, index)
              r5 = instantiate_node(SyntaxNode,input, index...(index + 1))
              @index += 1
            else
              terminal_parse_failure("$")
              r5 = nil
            end
            s0 << r5
            if r5
              s6, i6 = [], index
              loop do
                if has_terminal?('\G[^\\n\\\'\\"]', true, index)
                  r7 = true
                  @index += 1
                else
                  r7 = nil
                end
                if r7
                  s6 << r7
                else
                  break
                end
              end
              r6 = instantiate_node(SyntaxNode,input, i6...index, s6)
              s0 << r6
            end
          end
        end
      end
    end
    if s0.last
      r0 = instantiate_node(SMS,input, i0...index, s0)
      r0.extend(Sms0)
    else
      @index = i0
      r0 = nil
    end

    node_cache[:sms][start_index] = r0

    r0
  end

end

class NormfreeParser < Treetop::Runtime::CompiledParser
  include Normfree
end

