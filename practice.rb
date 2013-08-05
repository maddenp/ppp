#!/usr/bin/env ruby
#s="he'quoted'lld  a  ta o/ 'hello' ,11hHello world/!"
#s="helld  a  ta o/11hHello world/"
s=" program 'hello'"

def eat_whitespace(a,i)
  while a[@i]=~/[ \t]/
    @i+=1
  end
end

def check(a,i)
  while @i<=a.length
    b=a[@i]
    if b=~/d/
      data(a,@i)
    elsif b=~/f/
      format(a,@i)
    elsif b=~/c/
      call(a,@i)
    elsif b=~/'/
      single_quoted(a,@i)
    elsif b=~/"/
      double_quoted(a,@i)
    else
      @i+=1
    end
  end
  puts a.join
  exit
end

def single_quoted(a,i)
  @i+=1
  until a[@i]=~/'/
    @i+=1
  end
end

def double_quoted(a)
  @i+=1
  until a[@i]=~/"/
    @i+=1
  end
end

def data(a,i)
  @i+=1
  puts "FOUND D IN DATA"
  eat_whitespace(a,@i)
  if a[@i]=~/[aA]/
    @i+=1
    puts "FOUND A IN DATA"
    eat_whitespace(a,@i)
    if a[@i]=~/[tT]/
      @i+=1
      puts "FOUND T IN DATA"
      eat_whitespace(a,@i)
      if a[@i]=~/[aA]/
        @i+=1
        puts "FOUND A IN DATA"
        eat_whitespace(a,@i)
        eat_variable(a,@i)
        eat_whitespace(a,@i)
        if a[@i]=~/\//
          @i+=1
          eat_whitespace(a,@i)
          check_hollerith(a,@i)
        end
      end
    end
  end
end

def eat_variable(a,i)
  while a[@i]=~/[A-Za-z_1-9]/ # IS THERE ANYTHING ELSE THAT CAN BE IN A VARIABLE?
    @i+=1
  end
end
    
def hollerith(a,i)
  l=Array.new
  l[0]=a[@i]
  k=1
  while a[@i+1]=~/[0-9]/
    @i+=1
    l[k]=a[@i]
    k+=1
  end
  strlen=(l.join).to_i
  @i+=1
  if a[@i]=~/[Hh]/
    a=a.insert((@i+strlen+1),"#'#")
    a=a.insert((@i-k),"#'#")
    @i+=k+strlen+1
    check_hollerith(a,@i)
  end
end

def check_hollerith(a,i)
  while a[@i]!=/\//
    if a[@i]=~/\'/
      single_quoted(a,@i)
    elsif a[@i]=~/\"/
      double_quoted(a,@i)
    elsif a[@i]=~/[0-9]/
      hollerith(a,@i)
    end
    check(a,@i)
  end
end

def format
end

def call
end

@i=0
a=s.split(//)
check(a,@i)
