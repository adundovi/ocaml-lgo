        (* simple tuple map *)
        let map_2_tuple f (a, b) = (f a, f b)

        (* finds a needle in a string and return its position in the string *)
        let index_str needle str =
                let len_n = String.length needle in
                let len_s = String.length str in
                let rec loop i =
                        if (i + len_n) > len_s then -1
                        else let substr = String.sub str i len_n in
                        if needle = substr then i
                        else loop (i + 1)
                in
                loop 0

        (* splits an input string on a delimiter and returns the pieces
         * as a list;
         * comparable with String.split_on_chars *)
        let rec split_on_string str ~on:delimiter =
                let len_s, len_d = map_2_tuple String.length (str, delimiter) in
                let pos = index_str delimiter str in
                if (pos = -1) || ((pos + len_d) > len_s) then [str]
                else (String.sub str 0 (pos)) ::
                        (split_on_string ~on:delimiter
                                (String.sub
                                        str
                                        (pos + len_d)
                                        (len_s - (pos + len_d))
                                 )
                        )

        (* similar as above, only splits on the first occurence of a delimiter
         * and thus returns a tuple of two elements *)
        let split_on_first_string str ~on:delimiter =
                let len_s, len_d = map_2_tuple String.length (str, delimiter) in
                let pos = index_str delimiter str in
                if (pos = -1) || ((pos + len_d) > len_s) then [str]
                else [
                        (String.sub str 0 (pos));
                        (String.sub str (pos + len_d) (len_s - (pos + len_d)))
                ]
