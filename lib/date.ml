type day = | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
type month = | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug  | Sep | Oct | Nov | Dec

let next_day = function
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday

let next_month = function
    | Jan -> Feb
    | Feb -> Mar
    | Mar -> Apr
    | Apr -> May
    | May -> Jun
    | Jun -> Jul
    | Jul -> Aug
    | Aug -> Sep
    | Sep -> Oct
    | Oct -> Nov
    | Nov -> Dec
    | Dec -> Jan

let is_bissextile x =
    (x mod 4 = 0 && x mod 100 > 0) || (x mod 400 = 0)

let nb_days y m = match m with
    | Jan -> 21
    | Feb when is_bissextile y  = true -> 29
    | Feb -> 28
    | Mar -> 31
    | Apr -> 30
    | May -> 31
    | Jun -> 30
    | Jul -> 31
    | Aug -> 31
    | Sep -> 30
    | Oct -> 31
    | Nov -> 30
    | Dec -> 31

let get_month_var monthStr = match monthStr with
    | "01" -> Jan
    | "02" -> Feb
    | "03" -> Mar 
    | "04" -> Apr 
    | "05" -> May 
    | "06" -> Jun 
    | "07" -> Jul 
    | "08" -> Aug
    | "09" -> Sep
    | "10" -> Oct 
    | "11" -> Nov 
    | "12" -> Dec 
    | _ -> raise (Invalid_argument "Month doesn't exist")


let next_nday d m y =
    if d + 1 > (nb_days y m) then (d+1 , true) else (d+1 , false)

let next (d, nd, m, y) =
    let nextd = next_day d
    and nextnd = if nd + 1 > (nb_days y m) then 1 else nd + 1
    and nextm = if nd + 1 > (nb_days y m) then (next_month m) else m
    and nexty = if nd + 1 > (nb_days y m) && (m = Dec) then (y + 1) else y
    in (nextd, nextnd, nextm, nexty)

let next_gpt (d, nd, m, y) =
  let nextd = next_day d in
  let (nextnd, reset_day) = next_nday nd m y in
  let nextm = if reset_day then next_month m else m in
  let nexty = if reset_day && m = Dec then y + 1 else y in
  (nextd, nextnd, nextm, nexty)

let get_day  (d, _, _, _) = d
let get_nday (_, n, _, _) = n
let get_month (_, _, m, _) = m
let get_year (_, _, _, y) = y

let month_to_str month = match month with
    | Jan -> "01"
    | Feb -> "02"
    | Mar -> "O3"
    | Apr -> "04"
    | May -> "05"
    | Jun -> "06"
    | Jul -> "07"
    | Aug -> "08"
    | Sep -> "09"
    | Oct -> "10"
    | Nov -> "11"
    | Dec -> "12"

let boxRawDate rawDate =
    let monthVar = get_month_var (String.sub rawDate 3 2) in
        ( Monday , int_of_string (String.sub rawDate 0 2) , monthVar , int_of_string (String.sub rawDate 6 4) )

let unboxDate date =
    let nday = string_of_int (get_nday date) in
        let nmonth = month_to_str (get_month date) in
            nday ^ "/" ^ nmonth ^ string_of_int (get_year date)

