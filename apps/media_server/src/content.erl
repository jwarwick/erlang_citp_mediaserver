-module(content).
-compile([export_all]).

-define(THUMBNAIL_DIRECTORY, "/Users/jwarwick/Desktop/thumbnails").

server_name() ->
  "Erl MediaServer".

% return the list of known content
content_list() ->
  % [{LibraryNumber, FolderName, [ShowList]}, ...]
  [{0, "Movie Folder",
    % ShowList = {Number, MediaName, Timestamp, Width, Height, LengthFrames, FPS}
    [{0, "Movie 1", 0, 96, 72, 30, 30},
     {1, "Movie 2", 0, 96, 72, 30, 30}
    ]
   },
   {1, "Image Folder",
    [{0, "Image 1", 0, 96, 72, 30, 30}
    ]
   }
  ].

get_all_element_libraries() ->
  Content = content_list(),
  Result = [get_element_library(E) || E <- Content],
  {ok, Result}.

get_element_library({LibraryNumber, Name, ShowList}) ->
  {LibraryNumber, Name, length(ShowList)}.
                     
get_all_elements(LibraryNumber) ->
  Content = content_list(),
  {_LibNumber, _LibName, ShowList} = lists:nth(LibraryNumber + 1, Content),
  {ok, ShowList}.

get_thumbnail(Format, Width, Height, Flags, LibraryNumber, Element) ->
  Filename = string:to_lower(lists:flatten(io_lib:format("td_~B_~B.~s", [Width, Height, Format]))),
  Path = filename:join([?THUMBNAIL_DIRECTORY, Filename]),
  {ok, Buffer} = file:read_file(Path),
  {ok, Buffer}.
