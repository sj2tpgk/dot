function fish_greeting
  switch (date "+%a")
    case 土
      echo -n (set_color yellow)(date "+%Y-%m-%d") (set_color green)\((set_color blue)(date "+%a")(set_color green)\) (set_color cyan)(date "+%H:%M:%S")(set_color white)
    case 日
      echo -n (set_color yellow)(date "+%Y-%m-%d") (set_color green)\((set_color red)(date "+%a")(set_color green)\) (set_color cyan)(date "+%H:%M:%S")(set_color white)
    case '*'
      echo -n (set_color yellow)(date "+%Y-%m-%d") (set_color green)\((set_color white)(date "+%a")(set_color green)\) (set_color cyan)(date "+%H:%M:%S")(set_color white)
  end

  echo "  Welcome to" (set_color cyan)fish (set_color blue)'<\'))><'(set_color white)
end
