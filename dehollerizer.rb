class Dehollerizer

  def call_stmt
    fwd #PM# if invariant, move this into match
    if match "all"
      remove_continuation #PM# match does this, right?
      skip_variable
      remove_continuation
      check_hollerith_parens if see '('
    end
  end

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

  def data_stmt
    fwd
    if match "ata" and skip_variable and see '/'
      fwd
      check_hollerith_slashes
    end
  end

  def debug(x=nil)
    puts "DEBUG #{(x)?("[#{x}] "):("")}looking at: #{@a[@i..@i+5]}"
  end

  def format_stmt
    fwd
    check_hollerith_parens if match "ormat" and see '('
  end

  def fwd
    @i+=1
  end

  def mask_hollerith
    origin=@i
    digits=0
    l=[]
    while see /[0-9]/
      digits+=1
      l.push(see)
      fwd
      remove_whitespace
      remove_continuation
    end
    size=(l.join).to_i
    remove_whitespace
    if see 'h'
      @a[@i]='h'
      start=@i
      fwd
      while @i<=start+size
        remove_continuation
        fwd
      end
      hb=start-digits
      he=start+size
      hollerith=@a.join[hb..he]
      token=@m.set(hollerith)
      @a.slice!(hb..he)
      token.reverse.each_char { |c| @a.insert(hb,c) }
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
    @a=source.split(//)
    @m=stringmap
    while @i<=@a.length
      case see
      when "d" then data_stmt
      when "f" then format_stmt
      when "c" then call_stmt
      when "'" then skip_sq
      when '"' then skip_dq
      when "!" then skip_comment
      end
      fwd
    end
    @a.join
  end

  def remove_char
    @a.delete_at(@i)
  end

  def remove_comment
    if see "!"
      remove_char until see "\n"
    end
  end

  def remove_continuation
    skip_whitespace
    while see /[\&!]/
      start=@i
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
        @a.slice!(start..((see "&")?(@i):(@i-1)))
        @i=start
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
    return false unless (c=@a[@i])
    c=c.downcase
    return c unless x
    return (x.match(c))?(c):(false) if x.is_a?(Regexp)
    x.downcase!
    (Regexp.new(Regexp.quote(x)).match(c))?(c):(false)
  end
    
  def skip_comment
    fwd until see "\n" or @i==@a.size
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

end
