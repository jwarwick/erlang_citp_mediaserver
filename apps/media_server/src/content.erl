-module(content).
-compile([export_all]).

% return the list of known content
content_list() ->
  % [{FolderName, [ShowList]}, ...]
  [{"Folder 1",
    % MediaName, Width, Height, LengthFrames, FPS
    [{"Show 1", 96, 72, 30, 30},
     {"Show 2", 96, 72, 30, 30}
    ]
   },
   {"Folder 2",
    [{"2-2", 96, 72, 30, 30}
    ]
   }
  ].

get_all_element_libraries() ->
  Content = content_list(),
  % XXX
  ok.

get_all_elements(LibraryNumber) ->
  % XXX
  ok.


