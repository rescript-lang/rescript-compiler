(*
 * ALMABENCH 1.0.1
 *      OCaml     version
 *
 *     A number-crunching benchmark designed for cross-language and vendor
 *     comparisons.
 *
 *     Written by Shawn Wagner, from Scott Robert Ladd's versions for
 *      C++ and java.
 *
 *     No rights reserved. This is public domain software, for use by anyone.
 *
 *     This program calculates the daily ephemeris (at noon) for the years
 *     2000-2099 using an algorithm developed by J.L. Simon, P. Bretagnon, J.
 *     Chapront, M. Chapront-Touze, G. Francou and J. Laskar of the Bureau des
 *     Longitudes, Paris, France), as detailed in Astronomy & Astrophysics
 *     282, 663 (1994)
 *
 *    Note that the code herein is design for the purpose of testing
 *     computational performance; error handling and other such "niceties"
 *    is virtually non-existent.
 *
 *    Actual (and oft-updated) benchmark results can be found at:
 *            http://www.coyotegulch.com
 *
 *    Please do not use this information or algorithm in any way that might
 *    upset the balance of the universe or otherwise cause planets to impact
 *    upon one another.
 *)

let pic = 3.14159265358979323846
and j2000     = 2451545.0
and jcentury  = 36525.0
and jmillenia = 365250.0

let twopi     = 2.0 *. pic
and a2r       = pic /. 648000.0
and r2h       = 12.0 /. pic
and r2d       = 180.0 /. pic
and gaussk    = 0.01720209895

(* number of days to include in test *)
let test_loops  = 5  (* was: 20 *)
and test_length = 36525

(* sin and cos of j2000 mean obliquity (iau 1976) *)
and sineps = 0.3977771559319137
and coseps = 0.9174820620691818

and amas = [|  6023600.0; 408523.5; 328900.5; 3098710.0; 1047.355; 3498.5; 22869.0; 19314.0 |]

(*
 * tables giving the mean keplerian elements, limited to t**2 terms:
 *        a       semi-major axis (au)
 *        dlm     mean longitude (degree and arcsecond)
 *        e       eccentricity
 *        pi      longitude of the perihelion (degree and arcsecond)
 *        dinc    inclination (degree and arcsecond)
 *        omega   longitude of the ascending node (degree and arcsecond)
 *)
and a = [|
  [|   0.3870983098;             0.0;        0.0 |];
  [|   0.7233298200;             0.0;        0.0 |];
  [|   1.0000010178;             0.0;        0.0 |];
  [|   1.5236793419;         3e-10;        0.0 |];
  [|   5.2026032092;     19132e-10;  -39e-10 |];
  [|   9.5549091915; -0.0000213896;  444e-10 |];
  [|  19.2184460618;     -3716e-10;  979e-10 |];
  [|  30.1103868694;    -16635e-10;  686e-10 |] |]

and dlm =
  [| [| 252.25090552; 5381016286.88982;  -1.92789 |];
     [| 181.97980085; 2106641364.33548;   0.59381 |];
     [| 100.46645683; 1295977422.83429;  -2.04411 |];
     [| 355.43299958;  689050774.93988;   0.94264 |];
     [|  34.35151874;  109256603.77991; -30.60378 |];
     [|  50.07744430;   43996098.55732;  75.61614 |];
     [| 314.05500511;   15424811.93933;  -1.75083 |];
     [| 304.34866548;    7865503.20744;   0.21103 |] |]

and e =
  [| [|   0.2056317526;  0.0002040653;    -28349e-10 |];
     [|   0.0067719164; -0.0004776521;     98127e-10 |];
     [|   0.0167086342; -0.0004203654; -0.0000126734 |];
     [|   0.0934006477;  0.0009048438;    -80641e-10 |];
     [|   0.0484979255;  0.0016322542; -0.0000471366 |];
     [|   0.0555481426; -0.0034664062; -0.0000643639 |];
     [|   0.0463812221; -0.0002729293;  0.0000078913 |];
     [|   0.0094557470;  0.0000603263;           0.0 |] |]

and pi =
  [| [|  77.45611904;  5719.11590;   -4.83016 |];
     [| 131.56370300;   175.48640; -498.48184 |];
     [| 102.93734808; 11612.35290;   53.27577 |];
     [| 336.06023395; 15980.45908;  -62.32800 |];
     [|  14.33120687;  7758.75163;  259.95938 |];
     [|  93.05723748; 20395.49439;  190.25952 |];
     [| 173.00529106;  3215.56238;  -34.09288 |];
     [|  48.12027554;  1050.71912;   27.39717 |] |]
and dinc =
  [| [|   7.00498625; -214.25629;   0.28977 |];
     [|   3.39466189;  -30.84437; -11.67836 |];
     [|          0.0;  469.97289;  -3.35053 |];
     [|   1.84972648; -293.31722;  -8.11830 |];
     [|   1.30326698;  -71.55890;  11.95297 |];
     [|   2.48887878;   91.85195; -17.66225 |];
     [|   0.77319689;  -60.72723;   1.25759 |];
     [|   1.76995259;    8.12333;   0.08135 |] |]

and omega =
  [| [|  48.33089304;  -4515.21727;  -31.79892 |];
     [|  76.67992019; -10008.48154;  -51.32614 |];
     [| 174.87317577;  -8679.27034;   15.34191 |];
     [|  49.55809321; -10620.90088; -230.57416 |];
     [| 100.46440702;   6362.03561;  326.52178 |];
     [| 113.66550252;  -9240.19942;  -66.23743 |];
     [|  74.00595701;   2669.15033;  145.93964 |];
     [| 131.78405702;   -221.94322;   -0.78728 |] |]

(* tables for trigonometric terms to be added to the mean elements
   of the semi-major axes. *)
and kp =
  [| [| 69613.0;  75645.0; 88306.0; 59899.0; 15746.0; 71087.0; 142173.0;  3086.0;    0.0 |];
     [| 21863.0;  32794.0; 26934.0; 10931.0; 26250.0; 43725.0;  53867.0; 28939.0;    0.0 |];
     [| 16002.0;  21863.0; 32004.0; 10931.0; 14529.0; 16368.0;  15318.0; 32794.0;    0.0 |];
     [|  6345.0;   7818.0; 15636.0;  7077.0;  8184.0; 14163.0;   1107.0;  4872.0;    0.0 |];
     [|  1760.0;   1454.0;  1167.0;   880.0;   287.0;  2640.0;     19.0;  2047.0; 1454.0 |];
     [|   574.0;      0.0;   880.0;   287.0;    19.0;  1760.0;   1167.0;   306.0;  574.0 |];
     [|   204.0;      0.0;   177.0;  1265.0;     4.0;   385.0;    200.0;   208.0;  204.0 |];
     [|     0.0;    102.0;   106.0;     4.0;    98.0;  1367.0;    487.0;   204.0;    0.0 |] |]

and ca =
  [| [|       4.0;    -13.0;    11.0;    -9.0;    -9.0;    -3.0;    -1.0;     4.0;    0.0 |];
     [|    -156.0;     59.0;   -42.0;     6.0;    19.0;   -20.0;   -10.0;   -12.0;    0.0 |];
     [|      64.0;   -152.0;    62.0;    -8.0;    32.0;   -41.0;    19.0;   -11.0;    0.0 |];
     [|     124.0;    621.0;  -145.0;   208.0;    54.0;   -57.0;    30.0;    15.0;    0.0 |];
     [|  -23437.0;  -2634.0;  6601.0;  6259.0; -1507.0; -1821.0;  2620.0; -2115.0;-1489.0 |];
     [|   62911.0;-119919.0; 79336.0; 17814.0;-24241.0; 12068.0;  8306.0; -4893.0; 8902.0 |];
     [|  389061.0;-262125.0;-44088.0;  8387.0;-22976.0; -2093.0;  -615.0; -9720.0; 6633.0 |];
     [| -412235.0;-157046.0;-31430.0; 37817.0; -9740.0;   -13.0; -7449.0;  9644.0;    0.0 |] |]

and sa =
  [| [|     -29.0;    -1.0;     9.0;     6.0;    -6.0;     5.0;     4.0;     0.0;    0.0 |];
     [|     -48.0;  -125.0;   -26.0;   -37.0;    18.0;   -13.0;   -20.0;    -2.0;    0.0 |];
     [|    -150.0;   -46.0;    68.0;    54.0;    14.0;    24.0;   -28.0;    22.0;    0.0 |];
     [|    -621.0;   532.0;  -694.0;   -20.0;   192.0;   -94.0;    71.0;   -73.0;    0.0 |];
     [|  -14614.0;-19828.0; -5869.0;  1881.0; -4372.0; -2255.0;   782.0;   930.0;  913.0 |];
     [|  139737.0;     0.0; 24667.0; 51123.0; -5102.0;  7429.0; -4095.0; -1976.0;-9566.0 |];
     [| -138081.0;     0.0; 37205.0;-49039.0;-41901.0;-33872.0;-27037.0;-12474.0;18797.0 |];
     [|       0.0; 28492.0;133236.0; 69654.0; 52322.0;-49577.0;-26430.0; -3593.0;    0.0 |] |]

(* tables giving the trigonometric terms to be added to the mean elements of
   the mean longitudes . *)
and kq =
  [| [|  3086.0; 15746.0; 69613.0; 59899.0; 75645.0; 88306.0; 12661.0; 2658.0;  0.0;   0.0 |];
     [| 21863.0; 32794.0; 10931.0;    73.0;  4387.0; 26934.0;  1473.0; 2157.0;  0.0;   0.0 |];
     [|    10.0; 16002.0; 21863.0; 10931.0;  1473.0; 32004.0;  4387.0;   73.0;  0.0;   0.0 |];
     [|    10.0;  6345.0;  7818.0;  1107.0; 15636.0;  7077.0;  8184.0;  532.0; 10.0;   0.0 |];
     [|    19.0;  1760.0;  1454.0;   287.0;  1167.0;   880.0;   574.0; 2640.0; 19.0;1454.0 |];
     [|    19.0;   574.0;   287.0;   306.0;  1760.0;    12.0;    31.0;   38.0; 19.0; 574.0 |];
     [|     4.0;   204.0;   177.0;     8.0;    31.0;   200.0;  1265.0;  102.0;  4.0; 204.0 |];
     [|     4.0;   102.0;   106.0;     8.0;    98.0;  1367.0;   487.0;  204.0;  4.0; 102.0 |] |]

and cl =
  [| [|      21.0;   -95.0; -157.0;   41.0;   -5.0;   42.0;   23.0;   30.0;     0.0;    0.0 |];
     [|    -160.0;  -313.0; -235.0;   60.0;  -74.0;  -76.0;  -27.0;   34.0;     0.0;    0.0 |];
     [|    -325.0;  -322.0;  -79.0;  232.0;  -52.0;   97.0;   55.0;  -41.0;     0.0;    0.0 |];
     [|    2268.0;  -979.0;  802.0;  602.0; -668.0;  -33.0;  345.0;  201.0;   -55.0;    0.0 |];
     [|    7610.0; -4997.0;-7689.0;-5841.0;-2617.0; 1115.0; -748.0; -607.0;  6074.0;  354.0 |];
     [|  -18549.0; 30125.0;20012.0; -730.0;  824.0;   23.0; 1289.0; -352.0;-14767.0;-2062.0 |];
     [| -135245.0;-14594.0; 4197.0;-4030.0;-5630.0;-2898.0; 2540.0; -306.0;  2939.0; 1986.0 |];
     [|   89948.0;  2103.0; 8963.0; 2695.0; 3682.0; 1648.0;  866.0; -154.0; -1963.0; -283.0 |] |]

and sl =
  [| [|   -342.0;   136.0;  -23.0;   62.0;   66.0;  -52.0;  -33.0;   17.0;     0.0;    0.0 |];
     [|    524.0;  -149.0;  -35.0;  117.0;  151.0;  122.0;  -71.0;  -62.0;     0.0;    0.0 |];
     [|   -105.0;  -137.0;  258.0;   35.0; -116.0;  -88.0; -112.0;  -80.0;     0.0;    0.0 |];
     [|    854.0;  -205.0; -936.0; -240.0;  140.0; -341.0;  -97.0; -232.0;   536.0;    0.0 |];
     [| -56980.0;  8016.0; 1012.0; 1448.0;-3024.0;-3710.0;  318.0;  503.0;  3767.0;  577.0 |];
     [| 138606.0;-13478.0;-4964.0; 1441.0;-1319.0;-1482.0;  427.0; 1236.0; -9167.0;-1918.0 |];
     [|  71234.0;-41116.0; 5334.0;-4935.0;-1848.0;   66.0;  434.0;-1748.0;  3780.0; -701.0 |];
     [| -47645.0; 11647.0; 2166.0; 3194.0;  679.0;    0.0; -244.0; -419.0; -2531.0;   48.0 |] |]


(* Normalize angle into the range -pi <= A < +pi. *)
let anpm a =
  let w = mod_float a twopi in
    if abs_float w >= pic then begin
      if a < 0.0 then
        w +. twopi
      else
        w -. twopi
    end else
      w

(* The reference frame is equatorial and is with respect to the
 *    mean equator and equinox of epoch j2000. *)
let planetpv epoch np pv =
  (* time: julian millennia since j2000. *)
  let t = ((epoch.(0) -. j2000) +. epoch.(1)) /. jmillenia in
    (*  compute the mean elements. *)
  let da  = ref (a.(np).(0) +. (a.(np).(1) +. a.(np).(2) *. t ) *. t)
  and dl  = ref ((3600.0 *. dlm.(np).(0) +. (dlm.(np).(1) +. dlm.(np).(2) *. t ) *. t) *. a2r)
  and de  = e.(np).(0) +. (e.(np).(1) +. e.(np).(2) *. t ) *. t
  and dp  = anpm ((3600.0 *. pi.(np).(0) +. (pi.(np).(1) +. pi.(np).(2) *. t ) *. t ) *. a2r )
  and  di  = (3600.0 *. dinc.(np).(0) +. (dinc.(np).(1) +. dinc.(np).(2) *. t ) *. t ) *. a2r
  and doh = anpm ((3600.0 *. omega.(np).(0) +. (omega.(np).(1) +. omega.(np).(2) *. t ) *. t ) *. a2r )
              (* apply the trigonometric terms. *)
  and dmu = 0.35953620 *. t in

  (* loop invariant *)
  let kp = kp.(np) and kq = kq.(np) and ca = ca.(np) and sa = sa.(np)
  and cl = cl.(np) and sl = sl.(np) in
    for k = 0 to 7 do
      let arga = kp.(k) *. dmu
      and  argl = kq.(k) *. dmu in
        da := !da +. (ca.(k) *. cos arga +. sa.(k) *. sin arga) *. 0.0000001;
        dl := !dl +. (cl.(k) *. cos argl +. sl.(k) *. sin argl) *. 0.0000001
    done;
    begin let arga = kp.(8) *. dmu in
      da := !da +. t *. (ca.(8) *. cos arga  +. sa.(8) *. sin arga ) *. 0.0000001;
      for k = 8 to 9 do
        let argl = kq.(k) *. dmu in
          dl := !dl +. t *. ( cl.(k) *. cos argl +. sl.(k) *. sin argl ) *. 0.0000001
      done;
    end;


    dl := mod_float !dl twopi;

    (* iterative solution of kepler's equation to get eccentric anomaly. *)
    let am = !dl -. dp in
    let ae = ref (am +. de *. sin am)
    and k = ref 0 in
    let dae = ref ((am -. !ae +. de *. sin !ae) /. (1.0 -. de *. cos !ae)) in
      ae := !ae +. !dae;
      incr k;
      while !k < 10 or abs_float !dae >= 1e-12 do
        dae := (am -. !ae +. de *. sin !ae) /. (1.0 -. de *. cos !ae);
        ae := !ae +. !dae;
        incr k
      done;

      (* true anomaly. *)
      let ae2 = !ae /. 2.0 in
      let at  = 2.0 *. atan2 (sqrt ((1.0 +. de) /. (1.0 -. de)) *. sin ae2)  (cos ae2)
                  (* distance (au) and speed (radians per day). *)
      and r = !da *. (1.0 -. de *. cos !ae)
      and v = gaussk *. sqrt ((1.0 +. 1.0 /. amas.(np) ) /. (!da *. !da *. !da))
      and si2   = sin (di /. 2.0) in
      let xq = si2 *. cos doh
      and xp = si2 *. sin doh
      and tl = at +. dp in
      let xsw = sin tl
      and xcw = cos tl in
      let xm2  = 2.0 *. (xp *. xcw -. xq *. xsw )
      and xf = !da /. sqrt (1.0 -. de *. de)
      and ci2 = cos (di /. 2.0) in
      let xms = (de *. sin dp +. xsw) *. xf
      and xmc = (de *. cos dp  +. xcw) *. xf
      and xpxq2 = 2.0 *. xp *. xq in

      (* position (j2000 ecliptic x,y,z in au). *)
      let x = r *. (xcw -. xm2 *. xp)
      and y = r *. (xsw +. xm2 *. xq)
      and z = r *. (-.xm2 *. ci2) in

        (* rotate to equatorial. *)
        pv.(0).(0) <- x;
        pv.(0).(1) <- y *. coseps -. z *. sineps;
        pv.(0).(2) <- y *. sineps +. z *. coseps;

        (* velocity (j2000 ecliptic xdot,ydot,zdot in au/d). *)
        let x = v *. ((-1.0 +. 2.0 *. xp *. xp) *. xms +. xpxq2 *. xmc)
        and y = v *. (( 1.0 -. 2.0 *. xq *. xq ) *. xmc -. xpxq2 *. xms)
        and z = v *. (2.0 *. ci2 *. (xp *. xms +. xq *. xmc)) in

          (* rotate to equatorial *)
          pv.(1).(0) <- x;
          pv.(1).(1) <- y *. coseps -. z *. sineps;
          pv.(1).(2) <- y *. sineps +. z *. coseps


(* Computes RA, Declination, and distance from a state vector returned by
 * planetpv. *)
let radecdist state rdd =
  (* Distance *)
  rdd.(2) <- sqrt (state.(0).(0) *. state.(0).(0)
                 +. state.(0).(1) *. state.(0).(1)
                 +. state.(0).(2) *. state.(0).(2));
  (* RA *)
  rdd.(0) <- atan2 state.(0).(1) state.(0).(0) *. r2h;
  if rdd.(0) < 0.0 then rdd.(0) <- rdd.(0) +. 24.0;

  (* Declination *)
  rdd.(1) <- asin (state.(0).(2) /. rdd.(2)) *. r2d



(* Entry point. Calculate RA and Dec for noon on every day in 1900-2100 *)
let _ =
  let jd = [| 0.0; 0.0 |]
  and pv = [| [| 0.0; 0.0; 0.0 |]; [| 0.0; 0.0; 0.0 |] |]
  and position = [| 0.0; 0.0; 0.0 |] in
  (* Test *)
  jd.(0) <- j2000;
  jd.(1) <- 1.0;
  for p = 0 to 7 do
    planetpv jd p pv;
    radecdist pv position;
    Printf.printf "%d %.2f %.2f\n%!" p position.(0) position.(1)
  done;
  (* Benchmark *)
  for i = 0 to test_loops - 1 do
    jd.(0) <- j2000;
    jd.(1) <- 0.0;
    for n = 0 to test_length - 1 do
      jd.(0) <- jd.(0) +. 1.0;
      for p = 0 to 7 do
        planetpv jd p pv;
        radecdist pv position;
      done
    done
  done
