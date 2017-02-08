;;; hacker-typer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "hacker-typer" "hacker-typer.el" (22670 4377
;;;;;;  635052 10000))
;;; Generated autoloads from hacker-typer.el

(autoload 'hacker-typer "hacker-typer" "\
Start hacker-typer.

This function randomly selects a file from `hacker-typer-files',
downloading it if necessary, and creates an empty buffer in which
every keystroke types out some characters according to
`hacker-typer-type-rate', a la `http://hackertyper.com'. The
buffer is named after the file, with characters randomly
prepended.

If `hacker-typer-show-hackerman' is set to t, also show an
amusing picture of Rami Malek as \"hackerman\".

With prefix argument ARG, prompt for a file to type.

\(fn ARG)" t nil)

(autoload 'hacker-typer-quit "hacker-typer" "\
Kill ‘hacker-typer’ buffers.

\(fn)" t nil)

(autoload 'hacker-typer-hackerman "hacker-typer" "\
⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⢀⣀⣀⣀⣀⢀⡀⣀⣀⢀⡀⣀⢀⣀⣀⢀⢀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣻⣻⣯⢷⢺⣻⣟⢻⣟⣻⡟⣯⢻⣾⢟⢿⢺⣟⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢽⣻⣵⠞⠌⣁⣬⣬⣬⣫⣮⣭⡯⡗⢭⢽⢻⣟⢯⢯⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣟⢟⡞⢀⣤⣾⣿⣿⣿⣿⠿⢿⣶⣹⢹⡽⡻⣹⢺⢽⣯⢾⣟⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⢿⢇⣰⣷⣿⣿⡯⠩⠁⢀⠀⠀⠹⣿⣳⢪⣮⡗⢮⣯⢧⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣯⡽⡷⣿⣯⣿⣿⣿⡗⠂⣠⣀⣀⡀⣤⡤⣿⣮⣕⡖⢻⣟⣼⣿⣷⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣯⡿⣯⡷⣟⡟⣟⣿⣿⣿⣿⣿⡏⣿⣿⣿⠇⠈⠿⡛⢹⣿⡞⢮⢞⣺⣮⣾⣯⣾⢿⢿⣿⣿⣻⢯⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣟⣯⣿⣿⣿⣿⣿⢿⣽⣽⣿⣟⣿⣽⣯⣻⢯⡻⣟⢟⢟⣿⣿⣿⣿⣿⡣⠘⠊⠋⢀⣠⡀⠀⠀⣿⣯⣮⣟⣗⡽⡻⣞⢽⢻⣽⣽⣽⡿⣿⡟⣿⡷⣿⣿⣿⣻
⡮⡾⢽⡟⢻⡷⢻⣯⣯⣿⣯⢿⣟⣾⣻⣯⡧⡽⡯⣯⡽⡽⣟⣽⠿⣿⣿⣿⣿⢳⢣⢔⢀⢤⡦⢴⠂⢠⣿⣟⡖⢳⠚⣽⡽⡻⣻⢽⣾⢼⣾⢽⣷⢽⣯⣿⣻⢾⢟⣿
⡯⣟⣿⣟⣿⣿⣻⡷⣽⣼⣾⣽⣧⣯⣽⣼⣟⣯⢿⠗⡏⢯⣧⡞⢿⣿⣿⣿⣿⣿⣮⣱⡕⠵⠙⠋⠀⣼⣟⣟⣮⣯⣻⡳⣗⣟⢾⣽⣾⣽⣾⣽⣾⣿⣧⣯⣾⣽⣿⣷
⣟⣿⣽⣿⣾⣿⣿⣯⣿⣷⣻⣿⣿⣾⣾⣿⣿⢋⠥⣸⡔⡧⢲⡿⣻⣭⣽⣿⣿⣿⣿⣿⣿⣷⣿⣿⣿⡿⣻⡟⣻⣾⣽⢿⣟⡿⣿⣿⣾⣿⣻⣿⣾⣿⣿⣳⣾⣟⣿⣷
⣽⣯⣷⣿⣿⣿⣿⣿⣷⣿⣿⣟⣿⣾⣿⣟⢕⠹⣿⣧⣽⢯⣿⣽⡞⡲⣻⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣟⢿⣿⣿⣿⣿⣾⣿⣿⣿⣾⣿⣿⣿⢿⣿⣷⣿⣟
⣾⡽⣾⣿⣿⣿⣿⣯⣿⣿⣿⣿⣿⣿⣿⡯⢥⡗⣥⣫⡭⣟⢻⣿⣿⣟⣷⣧⢽⢾⣿⡿⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣽⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣾⣿⣿⣾
⣿⢿⣿⣻⣿⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⡯⢕⢝⡞⣲⣹⡎⡯⣟⡿⢾⣯⢿⡽⡻⡽⣿⢸⣿⣿⣿⣿⣞⢳⢝⣻⢿⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣷⣿⣿⣿⣿⣿⣿⡿⣻⣿⣿⣿⣿⣿⣿⡣⢬⣱⣎⡧⡕⡮⣕⣬⣽⣿⣽⣿⣟⣿⢻⡝⢺⣿⣟⣿⣯⢯⣮⣿⣧⢽⣼⣿⣿⣿⣿⣻⣿⣿⣿⣿⣿⣿⣿⣷⣽⣿⣿⣿
⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣫⢬⡞⡺⣹⡳⡼⡧⣯⡟⣏⣻⣿⣿⣿⣿⡗⣿⣿⣿⣯⣟⢯⣿⣿⣿⣿⣧⢿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣿
⣿⡿⣟⣯⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣣⢻⢯⣾⣷⣽⣯⣷⡟⡿⡿⣟⣏⣻⣟⣟⢿⣿⣿⣷⣿⣟⣿⣿⣿⣟⣯⣵⣿⣿⣿⣿⣿⣿⣮⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣽⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣯⣞⣿⣻⣟⣞⣾⢷⣾⣿⣿⣿⣿⣿⣿⣿⣷⣷⣯⣼⣻⣿⣿⣿⣿⣿⣿⣿⣟⣽⣿⣿⣿⣿⣿⣿⣿⣮⠻⣿⣟⡻⣿⣿⣟
⣿⣟⣿⣿⣿⣿⣿⣿⣿⣿⡿⣻⣿⣿⣿⣿⣿⣿⣽⣿⣽⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣻⣝⡻⣻⣻⣷⣻
⣿⣿⣿⣿⣿⣿⣿⣿⣿⢏⣾⣿⣿⣿⣿⣿⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⣛⣾⣽⣿⣼⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣝⢻⣿⣿
⣿⣿⣿⣿⣿⣿⣿⢿⣵⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣮⣷⣽⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣬⢿
⣿⣿⣿⣿⣿⡿⣣⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷
⣿⣿⣿⡿⣫⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣫⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⠟⣴⠟⠛⢻⡿⠛⠻⣿⣿⠟⠛⢻⡟⡿⠽⠛⠛⠛⠛⠛⠛⢻⠿⠛⠛⠻⠟⠛⠛⠛⢛⠛⠛⠛⠛⠻⢿⠟⠛⠛⢿⡿⠛⠈⠻⣿⣿⠟⠛⠻⣻⣿⠟⠋⠻⣿⠛⠛⢿
⣿⡏⠀⠀⠾⠀⠀⢸⡿⠁⠀⠀⠐⡟⠁⠀⢠⣤⣤⡔⠀⠀⠀⠀⣠⠄⠀⠀⠤⠀⠠⠄⠀⠀⠤⠀⠀⠈⠀⠀⠀⠟⠁⠀⠀⠠⣿⠁⠀⠀⠀⣵⠅⠀⠀⠀⠊⠀⠠⣾
⣿⠧⢤⣆⡀⠴⢤⠟⠴⢤⠤⠤⠄⠠⠤⠤⡿⡿⡿⠣⠤⠤⡴⠰⣗⠤⠤⠠⠠⠔⠀⠴⠔⡔⠠⠆⢠⠥⠤⠀⠤⠤⠄⠰⠄⠆⠰⠴⠤⠴⠤⢽⠣⠴⣤⠤⠤⢦⣼⣿
⣗⡀⣐⣿⡃⣀⠀⢀⣠⣶⣾⣆⠀⣠⣀⣀⠀⡀⣀⢀⣀⣧⡀⡀⣘⣄⢀⣀⢀⢀⣆⣀⣸⣿⣀⡀⢈⣀⢀⣃⣀⢠⡆⣀⢀⠀⡀⠶⣶⡀⡀⢘⢀⣀⡿⣄⡀⢀⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠄⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿

\(fn ARG)" t nil)

(defalias 'hackerman 'hacker-typer-hackerman)

(autoload 'hacker-typer-clear-cache "hacker-typer" "\
Delete all data in `hacker-typer-data-dir'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hacker-typer-autoloads.el ends here
