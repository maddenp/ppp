class Dehollerizer

# def call_stmt
#   @i+=1
#   eat
#   if @a[@i]=~/[Aa]/
#     @i+=1
#     eat
#     if @a[@i]=~/[Ll]/
#       @i+=1
#       eat
#       if @a[@i]=~/[Ll]/
#         @i+=1
#         eat
#         eat_variable
#         continuation
#         if @a[@i]=~/\(/
#           @i+=1
#           check_hollerith
#         end
#       end
#     end
#   end
# end

  def call_stmt
    fwd #PM# if invariant, move this into match
    if match("all")
      eat #PM# match does this, right?
      eat_variable
      continuation
      check_hollerith if @a[@i]=~/\(/ #PM# see
    end
  end

  def check_hollerith
    eat
    nest=0
    begin
      if see "'"        # Detect a single quoted string
        single_quoted   # Consume the string
      elsif see '"'     # Detect a double quoted string
        double_quoted   # Consume the string
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

  def check_hollerith_data  # Distinguishes between hollerith and other parts of a data statement
    eat
    until @a[@i]=~/\//      # '/' in a string will never be matched here
      if @a[@i]=~/\'/       # Detect a quoted string
        single_quoted       # Consumes everything in quotes
      elsif @a[@i]=~/\"/    # Detect a quoted string
        double_quoted       # Consumes everything in quotes
      elsif @a[@i]=~/[0-9]/ # Detect the hollerith lenght specifier
        hollerith           # Do work on hollerith
      end
      fwd                 # Advance to next character
    end
    process                 # Back to main check at the end of data statement
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
          @i=start                 # Move to next character after continuation
        else                       # If there is no matching &
          @a.slice!(start..(@i-1)) # Remove continuation
          @i=start                 # Move to next character after continuation
        end
      end
    end
  end

  def data_stmt           # Checks for all elements of a data statement
    fwd
    eat
    if @a[@i]=~/[Aa]/
      fwd
      eat
      if @a[@i]=~/[Tt]/
        fwd
        eat
        if @a[@i]=~/[Aa]/
          fwd
          eat
          eat_variable
          eat
          if @a[@i]=~/\//
            fwd
            check_hollerith_data
          end
        end
      end
    end
  end

  def debug(x=nil)
    $stderr.puts "### #{(x)?("[#{x}] "):("")}looking at: #{@a[@i..@i+5]}"
  end

  def double_quoted
    fwd                             # Advances to the next character (after ")
    until @a[@i]=~/"/
      fwd                           # Continue advancing until endquote is matched
    end
  end

  def eat
    eat_whitespace
    continuation
    true
  end

  def eat_variable
    if @a[@i]=~/[A-za-z]/
      fwd
      eat
      while @a[@i]=~/[A-Za-z0-9_]/
        fwd
        eat
      end
    end
  end

  def eat_whitespace
    fwd while @a[@i]=~/[ \t]/ # Consume all whitespace
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
    l=[]
    l[0]=@a[@i]                 # First digit in hollerith
    numdigits=1                 # Sets array value counter to one (zero is already taken)
    fwd                       # Moves to next character    
    continuation                # Check for a continuation after the first digit
    while @a[@i]=~/[0-9]/       # As long as the next character is a digit
      l[numdigits]=@a[@i]       # Sets next array value to hollerith digit value
      fwd                     # Move to next digit
      numdigits+=1              # Add one to the number of digits
      continuation              # Check for a continuation after the second digit
    end  
    strlen=(l.join).to_i        # Turns the array of digits into an integer
    remove_whitespace           # Removes whitespace after length specifier
    if @a[@i]=~/[Hh]/           # Check for an 'H' or 'h' after digits
      @a[@i]='h'                # Normalizes the 'H' to lowercase
      start=@i                  # Set a value for the character position of 'h'
      fwd                     # Move to the next character (first in string)
      while @i<=start+strlen    # While still in the string
        continuation            # Check for continuation at this point
        fwd                   # Move one forward
      end                       # Repeat until outside of the string
      hb=start-numdigits
      he=start+strlen
      hollerith=@a.join[hb..he] # Identify the hollerith
      token=@m.set(hollerith)
      @a.slice!(hb..he)
      token.each_char { |c| @a.insert(hb,c) }
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
        data_stmt      # Process potential data statement
      elsif b=~/[Ff]/  # Match start of a format statement
        format_stmt    # Process potential format statement
      elsif b=~/[Cc]/  # Looking for a call statement
        call_stmt      # Process potential call statement
      elsif b=~/'/     # Match single-quotes
        single_quoted  # Process quotes
        fwd          # Move ahead 
      elsif b=~/"/     # Match double quotes
        double_quoted  # Process quotes
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

  def see(x)
    return x.match(@a[@i].downcase) if x.is_a?(Regexp)
    Regexp.new(Regexp.quote(x)).match(@a[@i].downcase)
  end
    
  def single_quoted
    fwd                             # Advances to the next character (after ')
    until @a[@i]=~/'/
      fwd                           # Continue advancing until endquote is matched
    end
  end

  def skip_comment
    fwd until @a[@i]=="\n" or @i==@a.size
    fwd
  end

end
