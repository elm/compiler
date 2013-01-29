
(function() {
try{

var $op={};
for(this['i'] in Elm){eval('var '+this['i']+'=Elm[this.i];');}
if (Elm.Automaton) throw new Error("Module name collision, 'Automaton' is already defined."); 
Elm.Automaton=function(){
 try{
  if (!(Elm.Prelude instanceof Object)) throw 'module not found';
 } catch(e) {
  throw ("Module 'Prelude' is missing. Compile with --make flag or load missing module in a separate JavaScript file.");
 }
 var hiddenVars={};
 for (this['i'] in Elm.Prelude) {
  if (hiddenVars[this['i']]) continue;
  eval('var ' + this['i'] + ' = Elm.Prelude[this.i];');}
 function Automaton_0(a1){
  return ["Automaton",a1];}
 var Listen_8=["Listen"];
 var Ignore_9=["Ignore"];
 function DragFrom_10(a1){
  return ["DragFrom",a1];}
 $op['>>>'] = function(a1_24){
  return function(a2_25){
   return function(){
    var Automaton$m1_26=a1_24;
    var m1_27=function(){
    switch(Automaton$m1_26[0]){
     case "Automaton":
     return Automaton$m1_26[1];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    var Automaton$m2_28=a2_25;
    var m2_29=function(){
    switch(Automaton$m2_28[0]){
     case "Automaton":
     return Automaton$m2_28[1];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    return Automaton_0(function(a_32){
     return function(){
      var Tuple2$bm1__33=m1_27(a_32);
      var b_34=function(){
      switch(Tuple2$bm1__33[0]){
       case "Tuple2":
       return Tuple2$bm1__33[1];
      }
      throw new Error("Non-exhaustive pattern match in case");}();
      var m1__35=function(){
      switch(Tuple2$bm1__33[0]){
       case "Tuple2":
       return Tuple2$bm1__33[2];
      }
      throw new Error("Non-exhaustive pattern match in case");}();
      return function(){
       var Tuple2$cm2__40=m2_29(b_34);
       var c_41=function(){
       switch(Tuple2$cm2__40[0]){
        case "Tuple2":
        return Tuple2$cm2__40[1];
       }
       throw new Error("Non-exhaustive pattern match in case");}();
       var m2__42=function(){
       switch(Tuple2$cm2__40[0]){
        case "Tuple2":
        return Tuple2$cm2__40[2];
       }
       throw new Error("Non-exhaustive pattern match in case");}();
       return ["Tuple2",c_41,$op['>>>'](m1__35)(m2__42)];}();}();});}();};};
 $op['<<<'] = function(a2_47){
  return function(a1_48){
   return $op['>>>'](a1_48)(a2_47);};};
 $op['^>>'] = function(f_49){
  return function(a_50){
   return $op['>>>'](pure_4(f_49))(a_50);};};
 $op['>>^'] = function(a_51){
  return function(f_52){
   return $op['>>>'](a_51)(pure_4(f_52));};};
 $op['^<<'] = function(f_53){
  return function(a_54){
   return $op['>>>'](a_54)(pure_4(f_53));};};
 $op['<<^'] = function(a_55){
  return function(f_56){
   return $op['>>>'](pure_4(f_56))(a_55);};};
 var count_7=init_5(0)(function(__84){
  return function(c_85){
   return (1+c_85);};});
 function run_1(Automaton$m0_14){
  return function(input_15){
   return function(){
   switch(Automaton$m0_14[0]){
    case "Automaton":
    return lift(fst)(foldp$(function(a_17){
     return function(Tuple2$bAutomaton$m_18){
      return function(){
      switch(Tuple2$bAutomaton$m_18[0]){
       case "Tuple2":
       switch(Tuple2$bAutomaton$m_18[2][0]){
        case "Automaton":
        return Tuple2$bAutomaton$m_18[2][1](a_17);
       }break;
      }
      throw new Error("Non-exhaustive pattern match in case");}();};})(Automaton$m0_14[1])(input_15));
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function step_2(Automaton$m_21){
  return function(a_22){
   return function(){
   switch(Automaton$m_21[0]){
    case "Automaton":
    return Automaton$m_21[1](a_22);
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function combine_3(autos_57){
  return Automaton_0(function(a_58){
   return function(){
    var Tuple2$bsautos__59=unzip(map(function(Automaton$m_62){
     return function(){
     switch(Automaton$m_62[0]){
      case "Automaton":
      return Automaton$m_62[1](a_58);
     }
     throw new Error("Non-exhaustive pattern match in case");}();})(autos_57));
    var bs_60=function(){
    switch(Tuple2$bsautos__59[0]){
     case "Tuple2":
     return Tuple2$bsautos__59[1];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    var autos__61=function(){
    switch(Tuple2$bsautos__59[0]){
     case "Tuple2":
     return Tuple2$bsautos__59[2];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    return ["Tuple2",bs_60,combine_3(autos__61)];}();});}
 function pure_4(f_68){
  return Automaton_0(function(x_69){
   return ["Tuple2",f_68(x_69),pure_4(f_68)];});}
 function init_5(s_70){
  return function(step_71){
   return Automaton_0(function(a_72){
    return function(){
     var s__73=step_71(a_72)(s_70);
     return ["Tuple2",s__73,init_5(s__73)(step_71)];}();});};}
 function init__6(s_74){
  return function(step_75){
   return Automaton_0(function(a_76){
    return function(){
     var Tuple2$bs__77=step_75(a_76)(s_74);
     var b_78=function(){
     switch(Tuple2$bs__77[0]){
      case "Tuple2":
      return Tuple2$bs__77[1];
     }
     throw new Error("Non-exhaustive pattern match in case");}();
     var s__79=function(){
     switch(Tuple2$bs__77[0]){
      case "Tuple2":
      return Tuple2$bs__77[2];
     }
     throw new Error("Non-exhaustive pattern match in case");}();
     return ["Tuple2",b_78,init__6(s__79)(step_75)];}();});};}
 function vecSub_11(Tuple2$x1y1_86){
  return function(Tuple2$x2y2_87){
   return function(){
   switch(Tuple2$x1y1_86[0]){
    case "Tuple2":
    return function(){
    switch(Tuple2$x2y2_87[0]){
     case "Tuple2":
     return ["Tuple2",(Tuple2$x1y1_86[1]-Tuple2$x2y2_87[1]),(Tuple2$x1y1_86[2]-Tuple2$x2y2_87[2])];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function stepDrag_12(Tuple2$presspos_92){
  return function(Tuple2$dsform_93){
   return function(){
   switch(Tuple2$presspos_92[0]){
    case "Tuple2":
    return function(){
    switch(Tuple2$dsform_93[0]){
     case "Tuple2":
     return function(){
      function wrap_98(ds__99){
       return ["Tuple2",Tuple2$dsform_93[2],["Tuple2",ds__99,Tuple2$dsform_93[2]]];}
      return function(){
      switch(Tuple2$dsform_93[1][0]){
       case "DragFrom":
       return (Tuple2$presspos_92[1]?["Tuple2",uncurry(move)(vecSub_11(Tuple2$presspos_92[2])(Tuple2$dsform_93[1][1]))(Tuple2$dsform_93[2]),["Tuple2",DragFrom_10(Tuple2$dsform_93[1][1]),Tuple2$dsform_93[2]]]:function(){
        var form__101=uncurry(move)(vecSub_11(Tuple2$presspos_92[2])(Tuple2$dsform_93[1][1]))(Tuple2$dsform_93[2]);
        return ["Tuple2",form__101,["Tuple2",Listen_8,form__101]];}());
       case "Ignore":
       return wrap_98((Tuple2$presspos_92[1]?Ignore_9:Listen_8));
       case "Listen":
       return wrap_98((not(Tuple2$presspos_92[1])?Listen_8:(isWithin(Tuple2$presspos_92[2])(Tuple2$dsform_93[2])?DragFrom_10(Tuple2$presspos_92[2]):Ignore_9)));
      }
      throw new Error("Non-exhaustive pattern match in case");}();}();
    }
    throw new Error("Non-exhaustive pattern match in case");}();
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function draggable_13(form_102){
  return init__6(["Tuple2",Listen_8,form_102])(stepDrag_12);}
 return {$op : {'>>>' : $op['>>>'], '<<<' : $op['<<<'], '^>>' : $op['^>>'], '>>^' : $op['>>^'], '^<<' : $op['^<<'], '<<^' : $op['<<^']},
 run:run_1,
 step:step_2,
 combine:combine_3,
 pure:pure_4,
 init:init_5,
 init$:init__6,
 count:count_7,
 draggable:draggable_13};}();
Elm.main=function(){
 return Elm.Automaton.main;};
} catch (e) {
Elm.main=function() {
var msg = ('<br/><h2>Your browser may not be supported. Are you using a modern browser?</h2>' + '<br/><span style="color:grey">Runtime Error in Automaton module:<br/>' + e + '</span>');
document.body.innerHTML = Elm.Text.monospace(msg);throw e;};}}());