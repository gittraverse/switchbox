code= `#! /usr/bin/env ruby
# -*- coding: us-ascii -*-

$testnum=0
$ntest=0
$failed = 0
class Progress
  def initialize
    @color = nil
    @tty = nil
    @quiet = nil
    @verbose = nil
    ARGV.each do |arg|
      case arg
      when /\A--color(?:=(?:always|(auto)|(never)|(.*)))?\z/
        warn "unknown --color argument: #$3" if $3
        @color = $1 ? nil : !$2
      when /\A--tty(=(?:yes|(no)|(.*)))?\z/
        warn "unknown --tty argument: #$3" if $3
        @tty = !$1 || !$2
        true
      when /\A-(q|-quiet)\z/
        @quiet = true
      when /\A-(v|-verbose)\z/
        @verbose = true
      end
    end
    @tty = STDERR.tty? && !STDOUT.tty? && /dumb/ !~ ENV["TERM"] if @tty.nil?
    @eol = @tty && !@verbose ? "\r\e[K\r" : "\n"
    case @color
    when nil
      @color = @tty
    end
    if @color
      # dircolors-like style
      colors = (colors = ENV['TEST_COLORS']) ? Hash[colors.scan(/(\w+)=([^:\n]*)/)] : {}
      begin
        File.read(File.join(__dir__, "../test/colors")).scan(/(\w+)=([^:\n]*)/) do |n, c|
          colors[n] ||= c
        end
      rescue
      end
      @passed = "\e[;#{colors["pass"] || "32"}m"
      @failed = "\e[;#{colors["fail"] || "31"}m"
      @reset = "\e[m"
    else
      @passed = @failed = @reset = ""
    end
    extend(Rotator) if @tty
  end

  def passed_string
    "."
  end
  def failed_string
    "#{@failed}F#{@reset}"
  end
  def init_string
  end
  def finish_string
    if @quiet
      @eol
    else
      "#{@passed}#{@ok ? 'OK' : ''} #{$testnum}#{@reset}#{@eol}"
    end
  end
  def pass
    STDERR.print passed_string
  end
  def fail
    @ok = false
    STDERR.print failed_string
  end
  def init
    @ok = true
    STDERR.print init_string
  end
  def finish
    STDERR.print finish_string
  end

  module Rotator
    ROTATOR = %w[- \\ | /]
    BS = "\b" * ROTATOR[0].size
    def passed_string
      "#{BS}#{ROTATOR[(@count += 1) % ROTATOR.size]}"
    end
    def failed_string
      "#{BS}#{super}#{ROTATOR[@count % ROTATOR.size]}"
    end
    def init_string
      @count = 0
      " "
    end
    def finish_string
      s = "#{BS}#{' ' * BS.size}#{BS}#{super}"
      s.gsub!(/\n/, "\r\e[2K\r") if @quiet
      s
    end
  end
end
PROGRESS = Progress.new

def test_check(what)
  unless $ntest.zero?
    PROGRESS.finish
  end
  STDERR.print "#{$0}:#{what} "
  PROGRESS.init
  $what = what
  $testnum = 0
end

def test_ok(cond,n=1)
  $testnum+=1
  $ntest+=1
  where = (st = caller(n)) ? st[0] : "caller error! (n=#{n}, trace=#{caller(0).join(', ')}"
  if cond
    PROGRESS.pass
    printf "ok %d (%s)\n", $testnum, where
  else
    PROGRESS.fail
    printf "not ok %s %d -- %s\n", $what, $testnum, where
    $failed+=1
  end
  STDOUT.flush
  STDERR.flush
end`

var codeArray = code.split(/\n(?!""")/);
var classMatch = /class \w+/g;
var methodMatch = /def \w+/g;
var moc = [];
  for(var i = 0; i < codeArray.length; i++)
  {
  	if(codeArray[i].match(classMatch)){
  		var segment = codeArray[i].match(classMatch);
  		var curClass = segment[0].replace(/class /,"");
  		moc.push({line: i+1, name: curClass, type: "class"})
  	}
  	if(codeArray[i].match(methodMatch)){
  		var segment = codeArray[i].match(methodMatch);
  		var curMethod = segment[0].replace(/def /,"");
  		moc.push({line: i+1, name: curMethod, type: "method"})
  	}
  }
  console.log(codeArray);
  console.log(moc);
