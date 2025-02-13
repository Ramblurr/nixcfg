_inputs: _final: prev: {
  lib = prev.lib // {
    my = prev.lib.my // {
      uint =
        let
          num = prev.lib.std.num;
          optional = prev.lib.std.optional;
          string = prev.lib.std.string;
          set = prev.lib.std.set;
          list = prev.lib.std.list;

          inherit (num)
            rem
            ;

          toHex = num.toHexString;
          fromBaseDigits = num.fromBaseDigits;
        in
        rec {
          tryParse =
            x:
            let
              # deadnix: skip
              i = x num.tryParseInt x;
            in
            optional.match (num.tryParseInt x) {
              inherit (optional) nothing;
              just = i: optional.Iif (i >= 0) i;
            };
          parse =
            x:
            let
              i = num.parseInt x;
            in
            if i < 0 then throw "std.uint.parse: ${toString i} is negative" else i;
          Max = num.maxInt;
          Min = 0;

          toHexUpper = v: string.toUpper (toHex v);
          toHexLower = toHex;
          fromHex = s: fromBaseDigits 16 (list.map fromHexDigit (string.toChars s));

          hexCharsBase16 = "0123456789abcdef";

          fromHexDigit =
            let
              charsFor =
                str:
                list.imap (i: c: {
                  _0 = c;
                  _1 = i;
                }) (string.toChars str);
              chars = set.fromList (charsFor hexCharsBase16 ++ charsFor (string.toUpper hexCharsBase16));
            in
            d: chars.${d};

          # parseTime :: uint -> Timestamp
          # example: (parseTimestamp builtins.currentTime).y
          # https://stackoverflow.com/a/42936293
          parseTimestamp =
            s:
            let
              z = s / 86400 + 719468;
              era = (if z >= 0 then z else z - 146096) / 146097;
              doe = z - era * 146097;
              yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
              y = yoe + era * 400;
              doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
              mp = (5 * doy + 2) / 153;
              d = doy - (153 * mp + 2) / 5 + 1;
              m = mp + (if mp < 10 then 3 else -9);
              secondsInDay = rem s 86400;
            in
            {
              inherit doy d m;
              y = y + (if m <= 2 then 1 else 0);

              hours = secondsInDay / 3600;
              minutes = (rem secondsInDay 3600) / 60;
              seconds = rem s 60;
            };
        };
    };
  };
}
