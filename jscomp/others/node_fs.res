/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*** Node FS API */

/**
  Most fs functions let you omit the callback argument. If you do, a default
  callback is used that rethrows errors. To get a trace to the original call
  site, set the `NODE_DEBUG` environment variable:
*/
@module("fs")
external readdirSync: string => array<string> = "readdirSync"

@module("fs") external renameSync: (string, string) => unit = "renameSync"

type fd = private int

/**
  The relative path to a filename can be used. Remember, however, that this path
  will be relative to `process.cwd()`.
*/
type path = string

module Watch = {
  type t
  type config
  @obj
  external config: (
    ~persistent: bool=?,
    ~recursive: bool=?,
    ~encoding: Js_string.t=?,
    unit,
  ) => config = ""

  @module("fs")
  /**
    there is no need to accept listener, since we return a `watcher`
    back it can register event listener there .
    Currently we introduce a type `string_buffer`, for the
    `filename`, it will be `Buffer` when the encoding is `utf8.
    This is dependent type which can be tracked by GADT in some way,
    but to make things simple, let's just introduce an or type
  */
  external watch: (string, ~config: config=?, unit) => t = "watch"

  @send
  external on_: (
    t,
    @string
    [
      | #change((. string, Node.string_buffer) => unit)
      | #error((. unit) => unit)
    ],
  ) => t = "on"
  @send external close: t => unit = "close"
}

@module("fs") external ftruncateSync: (fd, int) => unit = "ftruncateSync"

@module("fs") external truncateSync: (string, int) => unit = "truncateSync"

@module("fs") external chownSync: (string, ~uid: int, ~gid: int) => unit = "chownSync"

@module("fs") external fchownSync: (fd, ~uid: int, ~gid: int) => unit = "fchownSync"

@module("fs") external readlinkSync: string => string = "readlinkSync"

@module("fs") external unlinkSync: string => unit = "unlinkSync"

@module("fs") external rmdirSync: string => unit = "rmdirSync"

/* TODO: `flags` support */
@module("fs")
external openSync: (
  path,
  @string
  [
    | @as("r") #Read
    | @as("r+") #Read_write
    | @as("rs+") #Read_write_sync
    | @as("w") #Write
    | @as("wx") #Write_fail_if_exists
    | @as("w+") #Write_read
    | @as("wx+") #Write_read_fail_if_exists
    | @as("a") #Append
    | @as("ax") #Append_fail_if_exists
    | @as("a+") #Append_read
    | @as("ax+") #Append_read_fail_if_exists
  ],
) => unit = "openSync"

type encoding = [
  | #hex
  | #utf8
  | #ascii
  | #latin1
  | #base64
  | #ucs2
  | #base64
  | #binary
  | #utf16le
]
@val @module("fs") external readFileSync: (string, encoding) => string = "readFileSync"

@val @module("fs") external readFileAsUtf8Sync: (string, @as("utf8") _) => string = "readFileSync"

@val @module("fs") external existsSync: string => bool = "existsSync"

@val @module("fs") external writeFileSync: (string, string, encoding) => unit = "writeFileSync"

@val @module("fs")
external writeFileAsUtf8Sync: (string, string, @as("utf8") _) => unit = "writeFileSync"
