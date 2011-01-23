#!/usr/bin/env ruby
=begin
make_suite.rb - extract oUnit tests from OCaml comments and print them to stdout

Copyright (C) 2007-2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.


USAGE:

ruby make_suite.rb filename.ml

where filename.ml contains comments like

(**T
  (* this test gets an automatic name of form "test_filename_line_X" *)
  "an expression that evaluates to true" <> ""
  "and another" <> ""
**)

(**T named_test
  foo = foo
**)

(*** named_raw_test_body
  assert_equal "foo" "foo";
  assert_equal "bar" "bar"
**)

(**Q
  (* Quickcheck laws *)
  Q.int (fun i -> i + i = i * 2)
  (Q.list Q.int) (fun l -> l = reverse (reverse l))
  (Q.list Q.int) (fun l -> length l = length (reverse l))
  (Q.list Q.int) (fun l -> let len = length l and rl = reverse l in (all id @@ mapWithIndex (fun e i -> rl !! (len-i-1) = e) l))
**)

The following Quickcheck generators are available:
  Q.unit
  Q.bool
  Q.int
  Q.pos_int
  Q.neg_int
  Q.float
  Q.pos_float
  Q.neg_float
  Q.char
  Q.printable_char
  Q.numeral_char
  Q.string_gen [_of_size]
  Q.string [_of_size]
  Q.printable_string [_of_size]
  Q.numeral_string [_of_size]
  Q.list [_of_size]
  Q.array [_of_size]
  Q.pair
  Q.triple 

The Quickcheck laws are expanded like this:
    The expression
  (Q.list Q.int) (fun l -> reverse (reverse l) = l)
    Becomes
  laws_exn "(Q.list Q.int) (fun l -> reverse (reverse l) = l)" (Q.list Q.int) (fun l -> reverse (reverse l) = l)

You can pass an optional count argument to all laws to control the amount of
test cases to generate:
  Q.int ~count:default_count (fun i -> i + i = i * 2)

List, string and array can additionally take a size function to control
the size of the generated collection:
  (Q.list_of_size (fun () -> Random.int 2) Q.int) (fun l -> reverse l = l)

=end

verbose = ARGV.delete("-v") != nil

if ARGV.size != 1
  puts "Usage: make_suite [-v] <file.ml>"
  exit 1
end

file = ARGV[0]

mod = File.basename(file)[/[^.]+/]
mod = mod.slice(0,1).capitalize + mod.slice(1..-1)

puts <<EOF
open OUnit
module Q = Quickcheck

let (==>) = Q.(==>)
let _iteri f l = ignore (List.fold_left (fun i v -> f v i; i + 1) 0 l)
let _TL = _iteri (fun (n,b) i ->
  OUnit.assert_bool ("Line " ^ string_of_int (i+1) ^ " of bool test: " ^ n) b)

open Batteries
open #{mod}

EOF

tests = []

data = IO.read(file)
data = data.gsub(/\(\*\*\T(.*?)\*\*\)/m){ |match|
  match[0,4] = "(***"
  lines = match.split(/\n/)
  head = "_TL [ "
  tail = "] "
    lines[1..-2] = lines[1..-2].map{|l| l.strip}.find_all{|l| not l.empty?}.map{|l|
    if not l =~ /^\(\*.*?\*\)$/
      "(#{l.dump}, (#{l}));"
    else
      l
    end
  }
    [ lines[0], head + lines[1..-2].join("\n      ") + tail, lines[-1] ].join("\n")
}

data = data.gsub(/\(\*\*\Q(.*?)\*\*\)/m){ |match|
  match[0,4] = "(***"
  lines = match.split(/\n/)
    lines[1..-2] = lines[1..-2].map{|l| l.strip}.find_all{|l| not l.empty?}.map{|l|
    if not l =~ /^\(\*.*?\*\)$/
      l.sub(/\S+/){|m| "Quickcheck.laws_exn #{l.strip.dump} "+m } + ";"
    else
      l
    end
  }
  [ lines[0], lines[1..-2].join("\n      ") + "()", lines[-1] ].join("\n")
}

auto_name = File.basename(file).downcase.
                 gsub(/^\d+|\.mli?$/,'').
                 gsub(/[^a-z0-9]/, '_')
data_lines = data.split(/\n/)
current_line_num = 0
data.scan(/\(\*\*\*\s(.*?)\*\*\)/m).each do |a|
  lines = a[0].split(/\n/)
  name, desc = lines.shift.strip.split(/\s+/)
  s = data_lines[current_line_num..-1].join("\n")
  index = s.index(a[0].strip)
  line_num = current_line_num + s[0...index].split(/\n/).size
  current_line_num = line_num + lines.size + 1
  name = "#{name ? name : "test"}_#{auto_name}_line_#{line_num}"
  desc ||= name
  tests << [name, desc]

  puts <<-EOF
let #{name} () = #{verbose ? "print_endline #{name.dump};" : ""}
#{lines.join("\n")}

  EOF
end

tests = tests.map{|name, desc| "    \"#{desc}\" >:: #{name}"}.join(";\n")

puts <<EOF

let suite = "#{mod} unit tests" >:::
  [
#{tests}
  ]

let () = Tests.register suite
EOF
