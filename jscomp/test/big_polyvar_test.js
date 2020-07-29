'use strict';


var _map = {"variant0":"variant0","variant1":"variant1","variant2":"variant2","variant3":"variant3","variant4":"variant4","variant5":"variant5","variant6":"variant6","variant7":"variant7","variant8":"variant8","variant9":"variant9","variant10":"variant10","variant11":"variant11","variant12":"variant12","variant13":"variant13","variant14":"variant14","variant15":"variant15","variant16":"variant16","variant17":"variant17","variant18":"variant18","variant19":"variant19","variant20":"variant20","variant21":"variant21","variant22":"variant22","variant23":"variant23","variant24":"variant24","variant25":"variant25","variant26":"variant26","variant27":"variant27","variant28":"variant28","variant29":"variant29","variant30":"variant30","variant31":"variant31","variant32":"variant32","variant33":"variant33","variant34":"variant34","variant35":"variant35","variant36":"variant36","variant37":"variant37","variant38":"variant38","variant39":"variant39","variant40":"variant40","variant41":"variant41","variant42":"variant42","variant43":"variant43","variant44":"variant44","variant45":"variant45","variant46":"variant46","variant47":"variant47","variant48":"variant48","variant49":"variant49","variant50":"variant50","variant51":"variant51","variant52":"variant52","variant53":"variant53","variant54":"variant54","variant55":"variant55","variant56":"variant56","variant57":"variant57","variant58":"variant58","variant59":"variant59","variant60":"variant60","variant61":"variant61","variant62":"variant62","variant63":"variant63","variant64":"variant64","variant65":"variant65","variant66":"variant66","variant67":"variant67","variant68":"variant68","variant69":"variant69","variant70":"variant70","variant71":"variant71","variant72":"variant72","variant73":"variant73","variant74":"variant74","variant75":"variant75","variant76":"variant76","variant77":"variant77","variant78":"variant78","variant79":"variant79","variant80":"variant80","variant81":"variant81","variant82":"variant82","variant83":"variant83","variant84":"variant84","variant85":"variant85","variant86":"variant86","variant87":"variant87","variant88":"variant88","variant89":"variant89","variant90":"variant90","variant91":"variant91","variant92":"variant92","variant93":"variant93","variant94":"variant94","variant95":"variant95","variant96":"variant96","variant97":"variant97","variant98":"variant98","variant99":"variant99","variant100":"variant100","variant101":"variant101","variant102":"variant102","variant103":"variant103","variant104":"variant104","variant105":"variant105","variant106":"variant106","variant107":"variant107","variant108":"variant108","variant109":"variant109","variant110":"variant110","variant111":"variant111","variant112":"variant112","variant113":"variant113","variant114":"variant114","variant115":"variant115","variant116":"variant116","variant117":"variant117","variant118":"variant118","variant119":"variant119","variant120":"variant120","variant121":"variant121","variant122":"variant122","variant123":"variant123","variant124":"variant124","variant125":"variant125","variant126":"variant126","variant127":"variant127","variant128":"variant128","variant129":"variant129","variant130":"variant130","variant131":"variant131","variant132":"variant132","variant133":"variant133","variant134":"variant134","variant135":"variant135","variant136":"variant136","variant137":"variant137","variant138":"variant138","variant139":"variant139","variant140":"variant140","variant141":"variant141","variant142":"variant142","variant143":"variant143","variant144":"variant144","variant145":"variant145","variant146":"variant146","variant147":"variant147","variant148":"variant148","variant149":"variant149","variant150":"variant150","variant151":"variant151","variant152":"variant152","variant153":"variant153","variant154":"variant154","variant155":"variant155","variant156":"variant156","variant157":"variant157","variant158":"variant158","variant159":"variant159","variant160":"variant160","variant161":"variant161","variant162":"variant162","variant163":"variant163","variant164":"variant164","variant165":"variant165","variant166":"variant166","variant167":"variant167","variant168":"variant168","variant169":"variant169","variant170":"variant170","variant171":"variant171","variant172":"variant172","variant173":"variant173","variant174":"variant174","variant175":"variant175","variant176":"variant176","variant177":"variant177","variant178":"variant178","variant179":"variant179","variant180":"variant180","variant181":"variant181","variant182":"variant182","variant183":"variant183","variant184":"variant184","variant185":"variant185","variant186":"variant186","variant187":"variant187","variant188":"variant188","variant189":"variant189","variant190":"variant190","variant191":"variant191","variant192":"variant192","variant193":"variant193","variant194":"variant194","variant195":"variant195","variant196":"variant196","variant197":"variant197","variant198":"variant198","variant199":"variant199","variant200":"variant200","variant201":"variant201","variant202":"variant202","variant203":"variant203","variant204":"variant204","variant205":"variant205","variant206":"variant206","variant207":"variant207","variant208":"variant208","variant209":"variant209","variant210":"variant210","variant211":"variant211","variant212":"variant212","variant213":"variant213","variant214":"variant214","variant215":"variant215","variant216":"variant216","variant217":"variant217","variant218":"variant218","variant219":"variant219","variant220":"variant220","variant221":"variant221","variant222":"variant222","variant223":"variant223","variant224":"variant224","variant225":"variant225","variant226":"variant226","variant227":"variant227","variant228":"variant228","variant229":"variant229","variant230":"variant230","variant231":"variant231","variant232":"variant232","variant233":"variant233","variant234":"variant234","variant235":"variant235","variant236":"variant236","variant237":"variant237","variant238":"variant238","variant239":"variant239","variant240":"variant240","variant241":"variant241","variant242":"variant242","variant243":"variant243","variant244":"variant244","variant245":"variant245","variant246":"variant246","variant247":"variant247","variant248":"variant248","variant249":"variant249","variant250":"variant250","variant251":"variant251","variant252":"variant252","variant253":"variant253","variant254":"variant254","variant255":"variant255","variant256":"variant256","variant257":"variant257","variant258":"variant258","variant259":"variant259","variant260":"variant260","variant261":"variant261","variant262":"variant262","variant263":"variant263","variant264":"variant264","variant265":"variant265","variant266":"variant266","variant267":"variant267","variant268":"variant268","variant269":"variant269","variant270":"variant270","variant271":"variant271","variant272":"variant272","variant273":"variant273","variant274":"variant274","variant275":"variant275","variant276":"variant276","variant277":"variant277","variant278":"variant278","variant279":"variant279","variant280":"variant280","variant281":"variant281","variant282":"variant282","variant283":"variant283","variant284":"variant284","variant285":"variant285","variant286":"variant286","variant287":"variant287","variant288":"variant288","variant289":"variant289","variant290":"variant290","variant291":"variant291","variant292":"variant292","variant293":"variant293","variant294":"variant294","variant295":"variant295","variant296":"variant296","variant297":"variant297","variant298":"variant298","variant299":"variant299"};

function tToJs(param) {
  return param;
}

function tFromJs(param) {
  return _map[param];
}

function eq(x, y) {
  if (x !== undefined) {
    if (y !== undefined) {
      return x === y;
    } else {
      return false;
    }
  } else {
    return y === undefined;
  }
}

if (!eq(tFromJs("variant0"), "variant0")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          610,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant1"), "variant1")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          611,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant2"), "variant2")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          612,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant3"), "variant3")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          613,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant4"), "variant4")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          614,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant5"), "variant5")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          615,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant6"), "variant6")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          616,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant7"), "variant7")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          617,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant8"), "variant8")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          618,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant9"), "variant9")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          619,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant10"), "variant10")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          620,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant11"), "variant11")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          621,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant12"), "variant12")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          622,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant13"), "variant13")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          623,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant14"), "variant14")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          624,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant15"), "variant15")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          625,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant16"), "variant16")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          626,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant17"), "variant17")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          627,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant18"), "variant18")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          628,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant19"), "variant19")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          629,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant20"), "variant20")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          630,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant21"), "variant21")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          631,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant22"), "variant22")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          632,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant23"), "variant23")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          633,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant24"), "variant24")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          634,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant25"), "variant25")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          635,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant26"), "variant26")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          636,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant27"), "variant27")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          637,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant28"), "variant28")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          638,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant29"), "variant29")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          639,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant30"), "variant30")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          640,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant31"), "variant31")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          641,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant32"), "variant32")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          642,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant33"), "variant33")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          643,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant34"), "variant34")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          644,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant35"), "variant35")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          645,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant36"), "variant36")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          646,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant37"), "variant37")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          647,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant38"), "variant38")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          648,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant39"), "variant39")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          649,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant40"), "variant40")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          650,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant41"), "variant41")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          651,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant42"), "variant42")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          652,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant43"), "variant43")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          653,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant44"), "variant44")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          654,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant45"), "variant45")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          655,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant46"), "variant46")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          656,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant47"), "variant47")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          657,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant48"), "variant48")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          658,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant49"), "variant49")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          659,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant50"), "variant50")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          660,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant51"), "variant51")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          661,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant52"), "variant52")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          662,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant53"), "variant53")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          663,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant54"), "variant54")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          664,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant55"), "variant55")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          665,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant56"), "variant56")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          666,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant57"), "variant57")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          667,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant58"), "variant58")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          668,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant59"), "variant59")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          669,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant60"), "variant60")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          670,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant61"), "variant61")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          671,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant62"), "variant62")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          672,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant63"), "variant63")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          673,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant64"), "variant64")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          674,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant65"), "variant65")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          675,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant66"), "variant66")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          676,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant67"), "variant67")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          677,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant68"), "variant68")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          678,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant69"), "variant69")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          679,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant70"), "variant70")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          680,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant71"), "variant71")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          681,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant72"), "variant72")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          682,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant73"), "variant73")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          683,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant74"), "variant74")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          684,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant75"), "variant75")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          685,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant76"), "variant76")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          686,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant77"), "variant77")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          687,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant78"), "variant78")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          688,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant79"), "variant79")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          689,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant80"), "variant80")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          690,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant81"), "variant81")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          691,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant82"), "variant82")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          692,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant83"), "variant83")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          693,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant84"), "variant84")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          694,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant85"), "variant85")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          695,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant86"), "variant86")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          696,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant87"), "variant87")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          697,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant88"), "variant88")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          698,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant89"), "variant89")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          699,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant90"), "variant90")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          700,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant91"), "variant91")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          701,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant92"), "variant92")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          702,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant93"), "variant93")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          703,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant94"), "variant94")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          704,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant95"), "variant95")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          705,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant96"), "variant96")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          706,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant97"), "variant97")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          707,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant98"), "variant98")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          708,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant99"), "variant99")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          709,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant100"), "variant100")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          710,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant101"), "variant101")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          711,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant102"), "variant102")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          712,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant103"), "variant103")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          713,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant104"), "variant104")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          714,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant105"), "variant105")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          715,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant106"), "variant106")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          716,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant107"), "variant107")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          717,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant108"), "variant108")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          718,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant109"), "variant109")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          719,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant110"), "variant110")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          720,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant111"), "variant111")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          721,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant112"), "variant112")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          722,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant113"), "variant113")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          723,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant114"), "variant114")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          724,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant115"), "variant115")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          725,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant116"), "variant116")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          726,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant117"), "variant117")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          727,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant118"), "variant118")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          728,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant119"), "variant119")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          729,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant120"), "variant120")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          730,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant121"), "variant121")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          731,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant122"), "variant122")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          732,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant123"), "variant123")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          733,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant124"), "variant124")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          734,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant125"), "variant125")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          735,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant126"), "variant126")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          736,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant127"), "variant127")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          737,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant128"), "variant128")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          738,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant129"), "variant129")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          739,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant130"), "variant130")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          740,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant131"), "variant131")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          741,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant132"), "variant132")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          742,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant133"), "variant133")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          743,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant134"), "variant134")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          744,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant135"), "variant135")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          745,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant136"), "variant136")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          746,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant137"), "variant137")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          747,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant138"), "variant138")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          748,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant139"), "variant139")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          749,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant140"), "variant140")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          750,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant141"), "variant141")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          751,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant142"), "variant142")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          752,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant143"), "variant143")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          753,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant144"), "variant144")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          754,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant145"), "variant145")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          755,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant146"), "variant146")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          756,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant147"), "variant147")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          757,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant148"), "variant148")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          758,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant149"), "variant149")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          759,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant150"), "variant150")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          760,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant151"), "variant151")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          761,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant152"), "variant152")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          762,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant153"), "variant153")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          763,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant154"), "variant154")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          764,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant155"), "variant155")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          765,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant156"), "variant156")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          766,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant157"), "variant157")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          767,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant158"), "variant158")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          768,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant159"), "variant159")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          769,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant160"), "variant160")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          770,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant161"), "variant161")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          771,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant162"), "variant162")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          772,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant163"), "variant163")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          773,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant164"), "variant164")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          774,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant165"), "variant165")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          775,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant166"), "variant166")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          776,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant167"), "variant167")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          777,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant168"), "variant168")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          778,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant169"), "variant169")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          779,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant170"), "variant170")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          780,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant171"), "variant171")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          781,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant172"), "variant172")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          782,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant173"), "variant173")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          783,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant174"), "variant174")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          784,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant175"), "variant175")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          785,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant176"), "variant176")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          786,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant177"), "variant177")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          787,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant178"), "variant178")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          788,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant179"), "variant179")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          789,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant180"), "variant180")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          790,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant181"), "variant181")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          791,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant182"), "variant182")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          792,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant183"), "variant183")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          793,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant184"), "variant184")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          794,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant185"), "variant185")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          795,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant186"), "variant186")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          796,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant187"), "variant187")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          797,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant188"), "variant188")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          798,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant189"), "variant189")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          799,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant190"), "variant190")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          800,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant191"), "variant191")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          801,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant192"), "variant192")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          802,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant193"), "variant193")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          803,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant194"), "variant194")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          804,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant195"), "variant195")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          805,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant196"), "variant196")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          806,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant197"), "variant197")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          807,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant198"), "variant198")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          808,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant199"), "variant199")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          809,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant200"), "variant200")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          810,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant201"), "variant201")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          811,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant202"), "variant202")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          812,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant203"), "variant203")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          813,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant204"), "variant204")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          814,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant205"), "variant205")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          815,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant206"), "variant206")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          816,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant207"), "variant207")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          817,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant208"), "variant208")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          818,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant209"), "variant209")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          819,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant210"), "variant210")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          820,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant211"), "variant211")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          821,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant212"), "variant212")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          822,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant213"), "variant213")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          823,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant214"), "variant214")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          824,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant215"), "variant215")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          825,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant216"), "variant216")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          826,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant217"), "variant217")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          827,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant218"), "variant218")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          828,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant219"), "variant219")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          829,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant220"), "variant220")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          830,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant221"), "variant221")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          831,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant222"), "variant222")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          832,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant223"), "variant223")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          833,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant224"), "variant224")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          834,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant225"), "variant225")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          835,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant226"), "variant226")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          836,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant227"), "variant227")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          837,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant228"), "variant228")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          838,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant229"), "variant229")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          839,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant230"), "variant230")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          840,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant231"), "variant231")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          841,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant232"), "variant232")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          842,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant233"), "variant233")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          843,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant234"), "variant234")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          844,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant235"), "variant235")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          845,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant236"), "variant236")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          846,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant237"), "variant237")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          847,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant238"), "variant238")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          848,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant239"), "variant239")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          849,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant240"), "variant240")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          850,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant241"), "variant241")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          851,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant242"), "variant242")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          852,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant243"), "variant243")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          853,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant244"), "variant244")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          854,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant245"), "variant245")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          855,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant246"), "variant246")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          856,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant247"), "variant247")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          857,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant248"), "variant248")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          858,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant249"), "variant249")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          859,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant250"), "variant250")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          860,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant251"), "variant251")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          861,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant252"), "variant252")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          862,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant253"), "variant253")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          863,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant254"), "variant254")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          864,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant255"), "variant255")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          865,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant256"), "variant256")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          866,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant257"), "variant257")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          867,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant258"), "variant258")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          868,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant259"), "variant259")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          869,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant260"), "variant260")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          870,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant261"), "variant261")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          871,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant262"), "variant262")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          872,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant263"), "variant263")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          873,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant264"), "variant264")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          874,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant265"), "variant265")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          875,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant266"), "variant266")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          876,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant267"), "variant267")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          877,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant268"), "variant268")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          878,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant269"), "variant269")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          879,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant270"), "variant270")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          880,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant271"), "variant271")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          881,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant272"), "variant272")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          882,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant273"), "variant273")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          883,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant274"), "variant274")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          884,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant275"), "variant275")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          885,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant276"), "variant276")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          886,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant277"), "variant277")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          887,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant278"), "variant278")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          888,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant279"), "variant279")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          889,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant280"), "variant280")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          890,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant281"), "variant281")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          891,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant282"), "variant282")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          892,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant283"), "variant283")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          893,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant284"), "variant284")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          894,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant285"), "variant285")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          895,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant286"), "variant286")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          896,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant287"), "variant287")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          897,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant288"), "variant288")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          898,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant289"), "variant289")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          899,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant290"), "variant290")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          900,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant291"), "variant291")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          901,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant292"), "variant292")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          902,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant293"), "variant293")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          903,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant294"), "variant294")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          904,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant295"), "variant295")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          905,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant296"), "variant296")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          906,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant297"), "variant297")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          907,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant298"), "variant298")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          908,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("variant299"), "variant299")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          909,
          2
        ],
        Error: new Error()
      };
}

if (!eq(tFromJs("xx"), undefined)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "big_polyvar_test.ml",
          910,
          2
        ],
        Error: new Error()
      };
}

exports.tToJs = tToJs;
exports.tFromJs = tFromJs;
exports.eq = eq;
/*  Not a pure module */
