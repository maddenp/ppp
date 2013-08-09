class Dehollerizer

  def check_hollerith_parens
    remove_continuation
    nestlevel=0
    begin
      case see
      when "'"     then skip_sq
      when '"'     then skip_dq
      when /[0-9]/ then mask_hollerith
      when "("     then nestlevel+=1
      when ")"     then nestlevel-=1
      end
      fwd
    end while nestlevel>0
  end

  def check_hollerith_slashes
    remove_continuation
    begin
      case see
      when "'"     then skip_sq
      when '"'     then skip_dq
      when /[0-9]/ then mask_hollerith
      end
      fwd
    end until see "/"
  end

  def debug(x=nil)
    lookahead=5
    puts "DEBUG #{(x)?("[#{x}] "):("")}: #{@s.split(//)[@i..@i+lookahead]}"
  end

  def fwd
    @i+=1
  end

  def mask_hollerith
    digits=[]
    origin=@i
    while see /[0-9]/
      digits.push(see)
      fwd
      remove_whitespace
      remove_continuation
    end
    length=(digits.join).to_i
    remove_whitespace
    if see 'h'
      @s[@i]='h'
      hindex=@i
      fwd
      while @i<=hindex+length
        remove_continuation
        fwd
      end
      p0=hindex-digits.size
      p1=hindex+length
      hollerith=@s[p0..p1]
      token=@m.set(hollerith)
      @s.slice!(p0..p1)
      token.reverse.each_char { |c| @s.insert(p0,c) }
      @i=origin+token.size-1
    else
      @i-=1
    end
  end

  def match(keyword)
    origin=@i
    keyword.each_char do |c|
      remove_continuation
      unless see c
        @i=origin
        return false
      end
      fwd
    end
    remove_continuation
  end

  def process(stringmap,source)
    @i=0
    @m=stringmap
    @s=source
    while @i<=@s.length
      case see
      when "c" then try_call
      when "d" then try_data
      when "f" then try_format
      when "'" then skip_sq
      when '"' then skip_dq
      when "!" then skip_comment
      end
      fwd
    end
    @s
  end

  def remove_char
    @s.slice!(@i)
  end

  def remove_comment
    if see "!"
      remove_char until see "\n"
    end
  end

  def remove_continuation
    skip_whitespace
    while see /[\&!]/
      origin=@i
      fwd
      remove_whitespace
      remove_comment
      if see "\n"
        remove_char
        while see /[ \t\n\!]/
          remove_whitespace
          remove_comment
          remove_newline
        end
        @s.slice!(origin..((see "&")?(@i):(@i-1)))
        @i=origin
      end
    end
    true
  end

  def remove_newline
    remove_char while see "\n"
  end

  def remove_whitespace
    remove_char while see /[ \t]/
  end

  def see(x=nil)
    return false unless (c=@s[@i])
    c=c.downcase
    return c unless x
    return (x.match(c))?(c):(false) if x.is_a?(Regexp)
    x.downcase!
    (Regexp.new(Regexp.quote(x)).match(c))?(c):(false)
  end

  def skip_comment
    fwd until see "\n" or @i==@s.length
    fwd
  end

  def skip_dq
    fwd until see '"'
  end

  def skip_sq
    fwd until see "'"
  end

  def skip_variable
    def advance
      fwd
      remove_continuation
    end
    return false unless see /[A-za-z]/
    advance
    advance while see /[A-Za-z0-9_]/
    true
  end

  def skip_whitespace
    fwd while see /[ \t]/
  end

  def try_call
    if match "call"
      skip_variable
      remove_continuation
      check_hollerith_parens if see '('
    end
  end

  def try_data
    if match "data" and skip_variable and see '/'
      fwd
      check_hollerith_slashes
    end
  end

  def try_format
    check_hollerith_parens if match "format" and see '('
  end

end
