class Dehollerizer

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
        @i+=1          # Move ahead 
      elsif b=~/"/     # Match double quotes
        double_quoted  # Process quotes
        @i+=1          # Move ahead
      elsif b=~/!/     # Match a comment (would not match quoted string
        skip_comment
      else
        @i+=1          # If none of the above is found, move on to next character
      end
    end
    @a.join
  end

  def eat
    eat_whitespace
    continuation
    true
  end

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
    @i+=1
    if match("all")
      eat
      eat_variable
      continuation
      if @a[@i]=~/\(/
        @i+=1
        check_hollerith
      end
    end
  end

  def data_stmt           # Checks for all elements of a data statement
    @i+=1
    eat
    if @a[@i]=~/[Aa]/
      @i+=1
      eat
      if @a[@i]=~/[Tt]/
        @i+=1
        eat
        if @a[@i]=~/[Aa]/
          @i+=1
          eat
          eat_variable
          eat
          if @a[@i]=~/\//
            @i+=1
            check_hollerith_data
          end
        end
      end
    end
  end

  def format_stmt              # Looks for all elements of a format statement
    @i+=1
    eat
    if @a[@i]=~/[Oo]/
      @i+=1
      eat
      if @a[@i]=~/[Rr]/
        @i+=1
        eat
        if @a[@i]=~/[Mm]/
          @i+=1
          eat
          if @a[@i]=~/[Aa]/
            @i+=1
            eat
            if @a[@i]=~/[Tt]/
              @i+=1
              eat
              if @a[@i]=~/\(/
                @i+=1
                eat
                check_hollerith
              end
            end
          end
        end
      end
    end
  end

  def match(keyword)
    reset=@i
    keyword.each_char do |c|
      eat
      unless @a[@i].downcase==c
        @i=reset
        return false
      end
      @i+=1
    end
    eat
  end
      
# def format_stmt              # Looks for all elements of a format statement
#   @i+=1
#   if match("ormat")
#     if @a[@i]=~/\(/
#       @i+=1
#       check_hollerith
#     end
#   end
# end

  def check_hollerith          # Distinguishes between hollerith and other parts of a format statment
    eat
    x=1                             # Parentheses counter (to find the end of the format statement)
    until x==0                      # Until all embedded parentheses are matched
      if @a[@i]=~/\'/                # Detect a single quoted string
        single_quoted         # Consume the string
      elsif @a[@i]=~/\"/             # Detect a double quoted string
        double_quoted         # Consume the string
      elsif @a[@i]=~/[0-9]/          # Detect the hollerith length specifier
        hollerith             # Do work on the hollerith
      elsif @a[@i]=~/\(/             # Detect open parentheses
        x+=1                        # Open parentheses: increase counter
      elsif @a[@i]=~/\)/             # Detect close parentheses
        x-=1                        # Close parentheses: decrease counter
      end
      @i+=1                         # Move to next character
    end
  end

  def check_hollerith_data      # Distinguishes between hollerith and other parts of a data statement
    eat
    until @a[@i]=~/\//                # '/' in a string will never be matched here
      if @a[@i]=~/\'/                 # Detect a quoted string
        single_quoted        # Consumes everything in quotes
      elsif @a[@i]=~/\"/              # Detect a quoted string
        double_quoted          # Consumes everything in quotes
      elsif @a[@i]=~/[0-9]/           # Detect the hollerith lenght specifier
        hollerith              # Do work on hollerith
      end
      @i+=1                          # Advance to next character
    end
    process                   # Back to main check at the end of data statement
  end

  def hollerith                         # Once a hollerith is found
    origin=@i
    l=Array.new                              # An array of digits specifying the length 
    l[0]=@a[@i]                               # First digit in hollerith
    numdigits=1                              # Sets array value counter to one (zero is already taken)
    @i+=1                                    # Moves to next character    
    continuation                       # Check for a continuation after the first digit
    while @a[@i]=~/[0-9]/                     # As long as the next character is a digit
      l[numdigits]=@a[@i]                     # Sets next array value to hollerith digit value
      @i+=1                                  # Move to next digit
      numdigits+=1                           # Add one to the number of digits
      continuation                     # Check for a continuation after the second digit
    end  
    strlen=(l.join).to_i                     # Turns the array of digits into an integer
    remove_whitespace                  # Removes whitespace after length specifier
    if @a[@i]=~/[Hh]/                         # Check for an 'H' or 'h' after digits
      @a[@i]='h'                              # Normalizes the 'H' to lowercase
      start=@i                               # Set a value for the character position of 'h'
      @i+=1                                  # Move to the next character (first in string)
      while @i<=start+strlen                 # While still in the string
        continuation                   # Check for continuation at this point
        @i+=1                                # Move one forward
      end                                               # Repeat until outside of the string
      hb=start-numdigits
      he=start+strlen
      hollerith=@a.join[hb..he]  # Identify the hollerith
      token=@m.set(hollerith)
      delta=hollerith.size-token.size
      @a.slice!(hb..he)
      @a.insert(hb,token)
      @i=origin
    else
      @i-=1                                  # Set the value to the last detected digit (moves forward later)
    end
  end

  def continuation
    while @a[@i]=~/[\&!]/            # Check for & or !
      start=@i                      # Designate a start index
      @i+=1                         # Move forward one (past &)
      remove_whitespace       # Remove space before a newline
      remove_comment          # Remove end of line comment
      if @a[@i]=~/\n/                # If end of line
        @a.delete_at(@i)             # Remove the newline character
        while @a[@i]=~/[ \t\n\!]/
          remove_whitespace   # Remove potential whitespace in next line
          remove_comment      # Remove potential comment in next line
          remove_newline      # Remove blank line
        end
        if @a[@i]=~/&/               # Find matching &
          @a.slice!(start..@i)       # Remove continuation
          @i=start                  # Move to next character after continuation
        else                        # If there is no matching &
          @a.slice!(start..(@i-1))   # Remove continuation
          @i=start                  # Move to next character after continuation
        end
      end
    end
  end

  def single_quoted
    @i+=1                             # Advances to the next character (after ')
    until @a[@i]=~/'/
      @i+=1                           # Continue advancing until endquote is matched
    end
  end

  def double_quoted
    @i+=1                             # Advances to the next character (after ")
    until @a[@i]=~/"/
      @i+=1                           # Continue advancing until endquote is matched
    end
  end

  def eat_whitespace
    @i+=1 while @a[@i]=~/[ \t]/       # Consume whitespace until all is gone
  end

  def eat_variable
    if @a[@i]=~/[A-za-z]/
      @i+=1
      eat
      while @a[@i]=~/[A-Za-z0-9_]/
        @i+=1
        eat
      end
    end
  end

  def remove_whitespace
    while @a[@i]=~/[ \t]/              # If character is whitespace
      @a.delete_at(@i)                 # Remove it and continue to check
    end
  end


  def remove_newline
    while @a[@i]=~/\n/                 # If character is newline
      @a.delete_at(@i)                 # Remove it and continue to check
    end
  end

  def skip_comment
    @i+=1 until @a[@i]=="\n" or @i==@a.size
    @i+=1
  end

  def remove_comment
    if @a[@i]=~/!/                     # If comment is detected
      until @a[@i]=~/\n/
        @a.delete_at(@i)               # Remove the entire comment
      end
    end
  end

end
