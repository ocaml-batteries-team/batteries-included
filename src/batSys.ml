(*
 * BatSys - additional and modified functions for System
 * Copyright (C) 1996 Xavier Leroy
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

include Sys

let files_of d = BatArray.enum (readdir d)

(* TODO: IMPLEMENT BIG_ENDIAN under 3.12.1 *)

let escape_arg_win arg =
  (* TODO: implement String.contains_any to improve performance by not escaping arguments that don't need it
 if not (String.contains_any " \t\n\"" arg) then
    arg
  else *)
    let b = Buffer.create (String.length arg) in
    Buffer.add_char b '"';
    let i = ref 0 in
    let backslash_count = ref 0 in
    while !i < String.length arg do
      backslash_count := 0;
      while !i < String.length arg && arg.[!i] = '\\' do
	incr i; incr backslash_count;
      done;
      if !i >= String.length arg then
	Buffer.add_string b (String.make (2 * !backslash_count) '\\')
      else if arg.[!i] = '"' then (
	Buffer.add_string b (String.make (2 * !backslash_count + 1) '\\');
	Buffer.add_char b '"';
	incr i;
      ) else (
	Buffer.add_string b (String.make !backslash_count '\\');
	Buffer.add_char b arg.[!i];
	incr i;
      )
    done;
    Buffer.add_char b '"';
    Buffer.contents b

(*$= escape_arg_win & ~cmp:BatString.equal ~printer:(fun x -> x)
"\"foo\"" (escape_arg_win "foo")
 "\"foo bar\"" (escape_arg_win "foo bar")
 "\"abc\\xyz\"" (escape_arg_win "abc\\xyz")
 *)




let escape_cmdline =
  if os_type = "Win32" then
    fun args -> List.map escape_arg_win args
  else
    (fun args -> args)

(* FROM http://blogs.msdn.com/b/twistylittlepassagesallalike/archive/2011/04/23/everyone-quotes-arguments-the-wrong-way.aspx

void
ArgvQuote (
    const std::wstring& Argument,
    std::wstring& CommandLine,
    bool Force
    )

/*++

Routine Description:

    This routine appends the given argument to a command line such
    that CommandLineToArgvW will return the argument string unchanged.
    Arguments in a command line should be separated by spaces; this
    function does not add these spaces.

Arguments:

    Argument - Supplies the argument to encode.

    CommandLine - Supplies the command line to which we append the encoded argument string.

    Force - Supplies an indication of whether we should quote
	    the argument even if it does not contain any characters that would
	    ordinarily require quoting.

Return Value:

    None.

Environment:

    Arbitrary.

--*/

{
    //
    // Unless we're told otherwise, don't quote unless we actually
    // need to do so --- hopefully avoid problems if programs won't
    // parse quotes properly
    //

    if (Force == false &&
	Argument.empty () == false &&
	Argument.find_first_of (L" \t\n\v\"") == Argument.npos)
    {
	CommandLine.append (Argument);
    }
    else {
	CommandLine.push_back (L'"');

	for (auto It = Argument.begin () ; ; ++It) {
	    unsigned NumberBackslashes = 0;

	    while (It != Argument.end () && *It == L'\\') {
		++It;
		++NumberBackslashes;
	    }

	    if (It == Argument.end ()) {

		//
		// Escape all backslashes, but let the terminating
		// double quotation mark we add below be interpreted
		// as a metacharacter.
		//

		CommandLine.append (NumberBackslashes * 2, L'\\');
		break;
	    }
	    else if ( *It == L'"') {

		//
		// Escape all backslashes and the following
		// double quotation mark.
		//

		CommandLine.append (NumberBackslashes * 2 + 1, L'\\');
		CommandLine.push_back ( *It);
	    }
	    else {

		//
		// Backslashes aren't special here.
		//

		CommandLine.append (NumberBackslashes, L'\\');
		CommandLine.push_back ( *It);
	    }
	}

	CommandLine.push_back (L'"');
    }
}
 *)
