class Dehollerizer

  def call_stmt
    fwd #PM# if invariant, move this into match
    if match("all")
      eat #PM# match does this, right?
      skip_variable
      continuation
      check_hollerith if see '('
    end
  end

  def check_hollerith #PM# merge with check_hollerith_data using passed in () or //
    eat
    nest=0
    begin
      if see "'"        # Detect a single quoted string
        skip_sq   # Consume the string
      elsif see '"'     # Detect a double quoted string
        skip_dq   # Consume the string
      elsif see /[0-9]/ # Detect the hollerith length specifier
        hollerith       # Do work on the hollerith
      elsif see "("     # Detect open parenthesis
        nest+=1         # Open parenthesis: increase nest
      elsif see ")"     # Detect close parenthesis
        nest-=1         # Close parenthesis: decrease nest
      end
      fwd               # Move to next character
    end while nest>0
  end

  def check_hollerith_data
    eat
    begin
      if @a[@i]=~/\'/       # Detect a quoted string
        skip_sq       # Consumes everything in quotes
      elsif @a[@i]=~/\"/    # Detect a quoted string
        skip_dq       # Consumes everything in quotes
      elsif @a[@i]=~/[0-9]/ # Detect the hollerith length specifier
        hollerith           # Do work on hollerith
      end
      fwd                 # Advance to next character
    end until @a[@i]=~/\//      # '/' in a string will never be matched here
  end

  def continuation
    while @a[@i]=~/[\&!]/          # Check for & or !
      start=@i                     # Designate a start index
      fwd                        # Move forward one (past &)
      remove_whitespace            # Remove space before a newline
      remove_comment               # Remove end of line comment
      if @a[@i]=~/\n/              # If end of line
        @a.delete_at(@i)           # Remove the newline character
        while @a[@i]=~/[ \t\n\!]/
          remove_whitespace        # Remove potential whitespace in next line
          remove_comment           # Remove potential comment in next line
          remove_newline           # Remove blank line
        end
        if @a[@i]=~/&/             # Find matching &
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
      check_hollerith_data
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
    check_hollerith if match ("ormat") and see '('
  end

  def fwd
    @i+=1
  end

  def hollerith                 # Once a hollerith is found
    origin=@i
    digits=0
    l=[]
    while see /[0-9]/
      digits+=1
      l.push(@a[@i]) #PM# see
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
      @a=source.split(//)   # Split string into array by character
      @m=stringmap
    end
    while @i<=@a.length
      b=@a[@i]
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
    if @a[@i]=~/!/        # If comment is detected
      until @a[@i]=~/\n/
        @a.delete_at(@i)  # Remove the entire comment
      end
    end
  end

  def remove_newline
    while @a[@i]=~/\n/    # If character is newline
      @a.delete_at(@i)    # Remove it and continue to check
    end
  end

  def remove_whitespace
    while @a[@i]=~/[ \t]/ # If character is whitespace
      @a.delete_at(@i)    # Remove it and continue to check
    end
  end

  def see(x) #PM# return either nil or the character pointed at, and use see in place of @a[@i] in general
    return nil if @a[@i].nil?
    return x.match(@a[@i].downcase) if x.is_a?(Regexp)
    x.downcase!
    Regexp.new(Regexp.quote(x)).match(@a[@i].downcase)
  end
    
  def skip_comment
    fwd until @a[@i]=="\n" or @i==@a.size
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
