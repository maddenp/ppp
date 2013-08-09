class Dehollerizer

  def call_stmt
    fwd #PM# if invariant, move this into match
    if match("all")
      eat #PM# match does this, right?
      skip_variable
      continuation
      check_hollerith_parens if see '('
    end
  end

  def check_hollerith_parens
    eat
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
    eat
    begin
      case see
      when "'"     then skip_sq
      when '"'     then skip_dq
      when /[0-9]/ then mask_hollerith
      end
      fwd
    end until see "/"
  end

  def continuation
    while see=~/[\&!]/          # Check for & or !
      start=@i                     # Designate a start index
      fwd                        # Move forward one (past &)
      remove_whitespace            # Remove space before a newline
      remove_comment               # Remove end of line comment
      if see=~/\n/              # If end of line
        @a.delete_at(@i)           # Remove the newline character
        while see=~/[ \t\n\!]/
          remove_whitespace        # Remove potential whitespace in next line
          remove_comment           # Remove potential comment in next line
          remove_newline           # Remove blank line
        end
        if see=~/&/             # Find matching &
          @a.slice!(start..@i)     # Remove continuation
        else                       # If there is no matching &
          @a.slice!(start..(@i-1)) # Remove continuation
        end
        @i=start                 # Move to next character after continuation
      end
    end
  end

  def data_stmt
    fwd
    if match("ata") and skip_variable and see '/'
      fwd #PM# can/should check_hollerith_data do this instead?
      check_hollerith_slashes
    end
  end

  def debug(x=nil)
    puts "DEBUG #{(x)?("[#{x}] "):("")}looking at: #{@a[@i..@i+5]}"
  end

  def eat
    skip_whitespace
    continuation
    true
  end

  def fwd_eat
    fwd
    eat
  end

  def format_stmt
    fwd
    check_hollerith_parens if match ("ormat") and see '('
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
      continuation #PM# remove_continuation
    end
    size=(l.join).to_i        # Turns the array of digits into an integer
    remove_whitespace           # Removes whitespace after length specifier
    if see 'h'                  # Check for an 'H' or 'h' after digits
      @a[@i]='h'                # Normalizes the 'H' to lowercase
      start=@i                  # Set a value for the character position of 'h'
      fwd                     # Move to the next character (first in string)
      while @i<=start+size    # While still in the string
        continuation            # Check for continuation at this point
        fwd                   # Move one forward
      end                       # Repeat until outside of the string
      hb=start-digits
      he=start+size
      hollerith=@a.join[hb..he] # Identify the hollerith
      token=@m.set(hollerith)
      @a.slice!(hb..he)
      token.reverse.each_char { |c| @a.insert(hb,c) }
      @i=origin+token.size-1
    else
      @i-=1                     # Set the value to the last detected digit (moves forward later)
    end
  end

  def match(keyword)
    origin=@i
    keyword.each_char do |c|
      eat
      unless see c
        @i=origin
        return false
      end
      fwd
    end
    eat
  end
      
  def process(stringmap=nil,source=nil)
    if stringmap
      @i=0
      @a=source.split(//) # string -> character array
      @m=stringmap
    end
    while @i<=@a.length
      b=see
      if b=~/[Dd]/     # Match start of a data statement
        data_stmt      # Handle potential data statement
      elsif b=~/[Ff]/  # Match start of a format statement
        format_stmt    # Handle potential format statement
      elsif b=~/[Cc]/  # Looking for a call statement
        call_stmt      # Handle potential call statement
      elsif b=~/'/     # Match single-quotes
        skip_sq  # Handle quotes
        fwd          # Move ahead 
      elsif b=~/"/     # Match double quotes
        skip_dq  # Handle quotes
        fwd          # Move ahead
      elsif b=~/!/     # Match a comment (would not match quoted string
        skip_comment
      else
        fwd          # If none of the above is found, move on to next character
      end
    end
    @a.join
  end

  def remove_comment
    if see=~/!/        # If comment is detected
      until see=~/\n/
        @a.delete_at(@i)  # Remove the entire comment
      end
    end
  end

  def remove_newline
    while see=~/\n/    # If character is newline
      @a.delete_at(@i)    # Remove it and continue to check
    end
  end

  def remove_whitespace
    while see=~/[ \t]/ # If character is whitespace
      @a.delete_at(@i)    # Remove it and continue to check
    end
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
    fwd
    fwd until see '"'
  end

  def skip_sq
    fwd
    fwd until see "'"
  end

  def skip_variable
    return false unless see /[A-za-z]/
    fwd_eat
    fwd_eat while see /[A-Za-z0-9_]/
    true
  end

  def skip_whitespace
    fwd while see /[ \t]/
  end

end
