#!/usr/bin/env ruby

s='hello'
def process(a,i)
  while @i<=a.length
    b=a[@i]
    if b=~/[Dd]/            # Match start of a data statement
      data_stmt(a,@i)       # Process potential data statement
    elsif b=~/[Ff]/         # Match start of a format statement
      format_stmt(a,@i)     # Process potential format statement
    elsif b=~/[Cc]/         # Looking for a call statement
      call_stmt(a,@i)       # Process potential call statement
    elsif b=~/'/            # Match single-quotes
      single_quoted(a,@i)   # Process quotes
      @i+=1                 # Move ahead 
    elsif b=~/"/            # Match double quotes
      double_quoted(a,@i)   # Process quotes
      @i+=1                 # Move ahead
    else
      @i+=1                 # If none of the above is found, move on to next character
    end
  end
  puts "\nNEW STRING:\n#{a.join}"
end

def data_stmt(a,i)               # Checks for all elements of a data statement
  @i+=1
  eat_whitespace(a,@i)
  if a[@i]=~/[aA]/
    @i+=1
    eat_whitespace(a,@i)
    continuation(a,@i)
    if a[@i]=~/[tT]/
      @i+=1
      eat_whitespace(a,@i)
      continuation(a,@i)
      if a[@i]=~/[aA]/
        @i+=1
        eat_whitespace(a,@i)
        continuation(a,@i)
        eat_variable(a,@i)
        continuation(a,@i)
        eat_whitespace(a,@i)
        continuation(a,@i)
        if a[@i]=~/\//
          @i+=1
          eat_whitespace(a,@i)
          continuation(a,@i)
          check_hollerith_data(a,@i)
        end
      end
    end
  end
end

def double_quoted(a,i)
  @i+=1                                 # Advances to the next character (after ")
  until a[@i]=~/"/
    @i+=1                               # Continue advancing until endquote is matched
  end
end

def eat_variable(a,i) # IS THERE ANYTHING ELSE THAT CAN BE IN A VARIABLE?
  while a[@i]=~/[A-Za-z_1-9]/           # Match a variable
    continuation(a,@i)                  # Check for a continuation in the variable
    @i+=1                               # Advance until the variable has ended
  end
end
    
def eat_whitespace(a,i)
  while a[@i]=~/[ \t]/                   # Only if the incoming character is whitespace
    @i+=1                                # Advance until the next character is not whitespace
  end
end

def single_quoted(a,i)
  @i+=1                                   # Advances to the next character (after ')
  until a[@i]=~/'/
    @i+=1                                 # Continue advancing until endquote is matched
  end
end

def hollerith(a,i)                        # Once a hollerith is found
  l=Array.new                             # An array of digits specifying the length 
  l[0]=a[@i]                              # First digit in hollerith
  numdigits=1                                # Sets array value counter to one (zero is already taken)
  @i+=1                                   # Moves to next character    
  continuation(a,@i)                      # Check for a continuation after the first digit
  while a[@i]=~/[0-9]/                    # As long as the next character is a digit
    l[numdigits]=a[@i]                       # Sets next array value to hollerith digit value
    @i+=1                                 # Move to next digit
    numdigits+=1                             # Add one to the number of digits
    continuation(a,@i)                    # Check for a continuation after the second digit
  end  
  strlen=(l.join).to_i                     # Turns the array of digits into an integer
  remove_whitespace(a,@i)                  # Removes whitespace after length specifier
  if a[@i]=~/[Hh]/                         # Check for an 'H' or 'h' after digits
    a[@i]='h'                              # Normalizes the 'H' to lowercase
    start=@i                               # Set a value for the character position of 'h'
    @i+=1                                  # Move to the next character (first in string)
    while @i<=start+strlen                 # While still in the string
      continuation(a,@i)                   # Check for continuation at this point
      @i+=1                                # Move one forward
    end                                               # Repeat until outside of the string
    hollerith=a.join[(start-numdigits)..(start+strlen)]  # Identify the hollerith
    puts "HOLLERITH:#{hollerith}"
    @i=start+strlen                               # Set the value to the last hollerith character (moves forward later) 
  end
end

def continuation(a,i)
  while a[@i]=~/\&/               # Check for &
    start=@i                      # Designate a start index
    @i+=1                         # Move forward one (past &)
    remove_whitespace(a,@i)       # Remove space before a newline
    remove_comment(a,@i)             # Remove end of line comment
    if a[@i]=~/\n/                # If end of line
      a.delete_at(@i)             # Remove the newline character
      while a[@i]=~/[ \t\n\!]/
        remove_whitespace(a,@i)   # Remove potential whitespace in next line
        remove_comment(a,@i)         # Remove potential comment in next line
        remove_newline(a,@i)         # Remove blank line
      end
      if a[@i]=~/&/               # Find matching &
        a.slice!(start..@i)       # Remove continuation
        @i=start                  # Move to next character after continuation
      else                        # If there is no matching &
        a.slice!(start..(@i-1))   # Remove continuation
        @i=start                  # Move to next character after continuation
      end
    end
  end
end

def remove_whitespace(a,i)
  while a[@i]=~/[ \t]/         # If character is whitespace
    a.delete_at(@i)            # Remove it and continue to check
  end
end


def remove_newline(a,i)
  while a[@i]=~/\n/            # If character is newline
    a.delete_at(@i)            # Remove it and continue to check
  end
end

def remove_comment(a,i)
  if a[@i]=~/!/               # If comment is detected
    until a[@i]=~/\n/
      a.delete_at(@i)         # Remove the entire comment
    end
  end
end

def check_hollerith_data(a,i)      # Distinguishes between hollerith and other parts of a data statement
  until a[@i]=~/\//                # '/' in a string will never be matched here
    if a[@i]=~/\'/                 # Detect a quoted string
        single_quoted(a,@i)        # Consumes everything in quotes
    elsif a[@i]=~/\"/              # Detect a quoted string
      double_quoted(a,@i)          # Consumes everything in quotes
    elsif a[@i]=~/[0-9]/           # Detect the hollerith lenght specifier
      hollerith(a,@i)              # Do work on hollerith
    end
    @i+=1                          # Advance to next character
  end
  process(a,@i)                      # Back to main check at the end of data statement
end

def check_hollerith(a,i)   # Distinguishes between hollerith and other parts of a format statment
  x=1                             # Parentheses counter (to find the end of the format statement)
  until x==0                      # Until all embedded parentheses are matched
    if a[@i]=~/\'/                # Detect a single quoted string
      single_quoted(a,@i)         # Consume the string
    elsif a[@i]=~/\"/             # Detect a double quoted string
      double_quoted(a,@i)         # Consume the string
    elsif a[@i]=~/[0-9]/          # Detect the hollerith length specifier
      hollerith(a,@i)             # Do work on the hollerith
    elsif a[@i]=~/\(/             # Detect open parentheses
      x+=1                        # Open parentheses: increase counter
    elsif a[@i]=~/\)/             # Detect close parentheses
      x-=1                        # Close parentheses: decrease counter
    end
    @i+=1                         # Move to next character
  end
  process(a,@i)
end

def format_stmt(a,i)           # Looks for all elements of a format statement
  @i+=1
  eat_whitespace(a,@i)
  continuation(a,@i)
  if a[@i]=~/[oO]/
    @i+=1
    eat_whitespace(a,@i)
    continuation(a,@i)
    if a[@i]=~/[rR]/
      @i+=1
      eat_whitespace(a,@i)
      continuation(a,@i)
      if a[@i]=~/[mM]/
        @i+=1
        eat_whitespace(a,@i)
        continuation(a,@i)
        if a[@i]=~/[aA]/
          @i+=1
          eat_whitespace(a,@i)
          continuation(a,@i)
          if a[@i]=~/[tT]/
            @i+=1
            eat_whitespace(a,@i)
            continuation(a,@i)
            if a[@i]=~/\(/
              @i+=1
              eat_whitespace(a,@i)
              continuation(a,@i)
              check_hollerith(a,@i)
            end
          end
        end
      end
    end
  end
end

def call_stmt(a,i)
  @i+=1                                   # Move past 'c'
  eat_whitespace(a,@i)                    # Consume potential whitespace
  continuation(a,@i)                      # Process potential continuation
  if a[@i]=~/[Aa]/                        # Match 'a'
    @i+=1                                 # Move past 'a'
    eat_whitespace(a,@i)                  # Consume potential whitespace
    continuation(a,@i)                    # Process potential continuation
    if a[@i]=~/[Ll]/                      # Match 'l'
      @i+=1                               # Move past 'l'
      eat_whitespace(a,@i)                # Consume potential whitespace
      continuation(a,@i)                  # Process potential continuation
      if a[@i]=~/[Ll]/                    # Match 'l'
        @i+=1                             # Move past 'l'
        eat_whitespace(a,@i)              # Consume potential whitespace
        continuation(a,@i)                # Process potential continuation
        eat_variable(a,@i)                # Process variable
        continuation(a,@i)                # Process potential continuation
        if a[@i]=~/\(/                    # Match '('
          @i+=1                           # Move past '('
          eat_whitespace(a,@i)            # Consume potential whitespace
          continuation(a,@i)              # Process potential continuation
          check_hollerith(a,@i)    # Check contents of statement for a hollerith
        end
      end
    end
  end
end

puts "ORIGINAL STRING:\n#{s}\n\n"
@i=0             # Character array counting variable
a=s.split(//)    # Split string into array by character
process(a,@i)      # Main process
