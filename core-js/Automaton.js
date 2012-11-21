Elm.Automaton=function(){
 for(this['i'] in Elm.Prelude){
     eval('var ' + this['i'] + '=Elm.Prelude[this.i];'); }
 function Automaton_0(a1){
  return ["Automaton",a1];};
 var Listen_9=["Listen"];
 var Ignore_10=["Ignore"];
 function DragFrom_11(a1){
  return ["DragFrom",a1];};
 var count_8=init_6(0)(function(__75){
  return function(c_76){
   return (1+c_76);};});
 function run_1(Automaton$m0_15){
  return function(input_16){
   return function(){
   switch(Automaton$m0_15[0]){
    case "Automaton":
    return lift(fst)(foldp_(function(a_18){
     return function(Tuple2$bAutomaton$m_19){
      return function(){
      switch(Tuple2$bAutomaton$m_19[0]){
       case "Tuple2":
       switch(Tuple2$bAutomaton$m_19[2][0]){
        case "Automaton":
        return Tuple2$bAutomaton$m_19[2][1](a_18);
       }break;
      }
      throw "Non-exhaustive pattern match in case";}();};})(Automaton$m0_15[1])(input_16));
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function step_2(Automaton$m_22){
  return function(a_23){
   return function(){
   switch(Automaton$m_22[0]){
    case "Automaton":
    return Automaton$m_22[1](a_23);
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function composeAuto_3(a1_25){
  return function(a2_26){
   return function(){
    var Automaton$m1_27=a1_25;
    var m1_28=function(){
    switch(Automaton$m1_27[0]){
     case "Automaton":
     return Automaton$m1_27[1];
    }
    throw "Non-exhaustive pattern match in case";}();
    var Automaton$m2_29=a2_26;
    var m2_30=function(){
    switch(Automaton$m2_29[0]){
     case "Automaton":
     return Automaton$m2_29[1];
    }
    throw "Non-exhaustive pattern match in case";}();
    return Automaton_0(function(a_33){
     return function(){
      var Tuple2$bm1__34=m1_28(a_33);
      var b_35=function(){
      switch(Tuple2$bm1__34[0]){
       case "Tuple2":
       return Tuple2$bm1__34[1];
      }
      throw "Non-exhaustive pattern match in case";}();
      var m1__36=function(){
      switch(Tuple2$bm1__34[0]){
       case "Tuple2":
       return Tuple2$bm1__34[2];
      }
      throw "Non-exhaustive pattern match in case";}();
      return function(){
       var Tuple2$cm2__41=m2_30(b_35);
       var c_42=function(){
       switch(Tuple2$cm2__41[0]){
        case "Tuple2":
        return Tuple2$cm2__41[1];
       }
       throw "Non-exhaustive pattern match in case";}();
       var m2__43=function(){
       switch(Tuple2$cm2__41[0]){
        case "Tuple2":
        return Tuple2$cm2__41[2];
       }
       throw "Non-exhaustive pattern match in case";}();
       return ["Tuple2",c_42,composeAuto_3(m1__36)(m2__43)];}();}();});}();};};
 function combine_4(autos_48){
  return Automaton_0(function(a_49){
   return function(){
    var Tuple2$bsautos__50=unzip(map(function(Automaton$m_53){
     return function(){
     switch(Automaton$m_53[0]){
      case "Automaton":
      return Automaton$m_53[1](a_49);
     }
     throw "Non-exhaustive pattern match in case";}();})(autos_48));
    var bs_51=function(){
    switch(Tuple2$bsautos__50[0]){
     case "Tuple2":
     return Tuple2$bsautos__50[1];
    }
    throw "Non-exhaustive pattern match in case";}();
    var autos__52=function(){
    switch(Tuple2$bsautos__50[0]){
     case "Tuple2":
     return Tuple2$bsautos__50[2];
    }
    throw "Non-exhaustive pattern match in case";}();
    return ["Tuple2",bs_51,combine_4(autos__52)];}();});};
 function pure_5(f_59){
  return Automaton_0(function(x_60){
   return ["Tuple2",f_59(x_60),pure_5(f_59)];});};
 function init_6(s_61){
  return function(step_62){
   return Automaton_0(function(a_63){
    return function(){
     var s__64=step_62(a_63)(s_61);
     return ["Tuple2",s__64,init_6(s__64)(step_62)];}();});};};
 function init__7(s_65){
  return function(step_66){
   return Automaton_0(function(a_67){
    return function(){
     var Tuple2$bs__68=step_66(a_67)(s_65);
     var b_69=function(){
     switch(Tuple2$bs__68[0]){
      case "Tuple2":
      return Tuple2$bs__68[1];
     }
     throw "Non-exhaustive pattern match in case";}();
     var s__70=function(){
     switch(Tuple2$bs__68[0]){
      case "Tuple2":
      return Tuple2$bs__68[2];
     }
     throw "Non-exhaustive pattern match in case";}();
     return ["Tuple2",b_69,init__7(s__70)(step_66)];}();});};};
 function vecSub_12(Tuple2$x1y1_77){
  return function(Tuple2$x2y2_78){
   return function(){
   switch(Tuple2$x1y1_77[0]){
    case "Tuple2":
    return function(){
    switch(Tuple2$x2y2_78[0]){
     case "Tuple2":
     return ["Tuple2",(Tuple2$x1y1_77[1]-Tuple2$x2y2_78[1]),(Tuple2$x1y1_77[2]-Tuple2$x2y2_78[2])];
    }
    throw "Non-exhaustive pattern match in case";}();
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function stepDrag_13(Tuple2$presspos_83){
  return function(Tuple2$dsform_84){
   return function(){
   switch(Tuple2$presspos_83[0]){
    case "Tuple2":
    return function(){
    switch(Tuple2$dsform_84[0]){
     case "Tuple2":
     return function(){
      function wrap_89(ds__90){
       return ["Tuple2",Tuple2$dsform_84[2],["Tuple2",ds__90,Tuple2$dsform_84[2]]];};
      return function(){
      switch(Tuple2$dsform_84[1][0]){
       case "DragFrom":
       return (Tuple2$presspos_83[1]?["Tuple2",uncurry(move)(vecSub_12(Tuple2$presspos_83[2])(Tuple2$dsform_84[1][1]))(Tuple2$dsform_84[2]),["Tuple2",DragFrom_11(Tuple2$dsform_84[1][1]),Tuple2$dsform_84[2]]]:function(){
        var form__92=uncurry(move)(vecSub_12(Tuple2$presspos_83[2])(Tuple2$dsform_84[1][1]))(Tuple2$dsform_84[2]);
        return ["Tuple2",form__92,["Tuple2",Listen_9,form__92]];}());
       case "Ignore":
       return wrap_89((Tuple2$presspos_83[1]?Ignore_10:Listen_9));
       case "Listen":
       return wrap_89((not(Tuple2$presspos_83[1])?Listen_9:(isWithin(Tuple2$presspos_83[2])(Tuple2$dsform_84[2])?DragFrom_11(Tuple2$presspos_83[2]):Ignore_10)));
      }
      throw "Non-exhaustive pattern match in case";}();}();
    }
    throw "Non-exhaustive pattern match in case";}();
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function draggable_14(form_93){
  return init__7(["Tuple2",Listen_9,form_93])(stepDrag_13);};
 return {Automaton:Automaton_0,run:run_1,step:step_2,composeAuto:composeAuto_3,combine:combine_4,pure:pure_5,init:init_6,init_:init__7,count:count_8,Listen:Listen_9,Ignore:Ignore_10,DragFrom:DragFrom_11,vecSub:vecSub_12,stepDrag:stepDrag_13,draggable:draggable_14};}();
