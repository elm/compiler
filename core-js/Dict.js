
try{

for(var i in Elm) { this[i] = Elm[i]; }
if (Elm.Dict) throw "Module name collision, 'Dict' is already defined."; 
Elm.Dict=function(){
 try{if (!(Elm.Prelude instanceof Object)) throw 'module not found'; } catch(e) {throw "Module 'Prelude' is missing. Compile with --make flag or load missing module in a separate JavaScript file.";}
 var hiddenVars=[];
 for(var i in Elm.Prelude){
  if (hiddenVars.indexOf(i) >= 0) continue;
  this[i]=Elm.Prelude[i];}
 try{if (!(Elm.Maybe instanceof Object)) throw 'module not found'; } catch(e) {throw "Module 'Maybe' is missing. Compile with --make flag or load missing module in a separate JavaScript file.";}
 var isJust=Elm.Maybe.isJust;
 var Red_0=["Red"];
 var Black_1=["Black"];
 function RBNode_2(a1){
  return function(a2){
   return function(a3){
    return function(a4){
     return function(a5){
      return ["RBNode",a1,a2,a3,a4,a5];};};};};};
 var RBEmpty_3=["RBEmpty"];
 var empty_4=RBEmpty_3;
 var raise_5=console.log;
 function min_6(t_43){
  return function(){
  switch(t_43[0]){
   case "RBEmpty":
   return raise_5(Value.str("(min RBEmpty) is not defined"));
   case "RBNode":
   switch(t_43[4][0]){
    case "RBEmpty":
    return ["Tuple2",t_43[2],t_43[3]];
   }
   return min_6(t_43[4]);
  }
  throw "Non-exhaustive pattern match in case";}();};
 function lookup_7(k_47){
  return function(t_48){
   return function(){
   switch(t_48[0]){
    case "RBEmpty":
    return Nothing;
    case "RBNode":
    return function(){
    var case6=compare(k_47)(t_48[2]);
    switch(case6[0]){
     case "EQ":
     return Just(t_48[3]);
     case "GT":
     return lookup_7(k_47)(t_48[5]);
     case "LT":
     return lookup_7(k_47)(t_48[4]);
    }
    throw "Non-exhaustive pattern match in case";}();
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function findWithDefault_8(base_53){
  return function(k_54){
   return function(t_55){
    return function(){
    switch(t_55[0]){
     case "RBEmpty":
     return base_53;
     case "RBNode":
     return function(){
     var case6=compare(k_54)(t_55[2]);
     switch(case6[0]){
      case "EQ":
      return t_55[3];
      case "GT":
      return findWithDefault_8(base_53)(k_54)(t_55[5]);
      case "LT":
      return findWithDefault_8(base_53)(k_54)(t_55[4]);
     }
     throw "Non-exhaustive pattern match in case";}();
    }
    throw "Non-exhaustive pattern match in case";}();};};};
 function find_9(k_60){
  return function(t_61){
   return function(){
   switch(t_61[0]){
    case "RBEmpty":
    return raise_5(Value.str("Key was not found in dictionary!"));
    case "RBNode":
    return function(){
    var case6=compare(k_60)(t_61[2]);
    switch(case6[0]){
     case "EQ":
     return t_61[3];
     case "GT":
     return find_9(k_60)(t_61[5]);
     case "LT":
     return find_9(k_60)(t_61[4]);
    }
    throw "Non-exhaustive pattern match in case";}();
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function member_10(k_66){
  return function(t_67){
   return isJust(lookup_7(k_66)(t_67));};};
 function rotateLeft_11(t_68){
  return function(){
  switch(t_68[0]){
   case "RBNode":
   switch(t_68[5][0]){
    case "RBNode":
    return RBNode_2(t_68[1])(t_68[5][2])(t_68[5][3])(RBNode_2(Red_0)(t_68[2])(t_68[3])(t_68[4])(t_68[5][4]))(t_68[5][5]);
   }break;
  }
  return raise_5(Value.str("rotateLeft of a node without enough children"));}();};
 function rotateRight_12(t_78){
  return function(){
  switch(t_78[0]){
   case "RBNode":
   switch(t_78[4][0]){
    case "RBNode":
    return RBNode_2(t_78[1])(t_78[4][2])(t_78[4][3])(t_78[4][4])(RBNode_2(Red_0)(t_78[2])(t_78[3])(t_78[4][5])(t_78[5]));
   }break;
  }
  return raise_5(Value.str("rotateRight of a node without enough children"));}();};
 function rotateLeftIfNeeded_13(t_88){
  return function(){
  switch(t_88[0]){
   case "RBNode":
   switch(t_88[5][0]){
    case "RBNode":
    switch(t_88[5][1][0]){
     case "Red":
     return rotateLeft_11(t_88);
    }break;
   }break;
  }
  return t_88;}();};
 function rotateRightIfNeeded_14(t_89){
  return function(){
  switch(t_89[0]){
   case "RBNode":
   switch(t_89[4][0]){
    case "RBNode":
    switch(t_89[4][1][0]){
     case "Red":
     switch(t_89[4][4][0]){
      case "RBNode":
      switch(t_89[4][4][1][0]){
       case "Red":
       return rotateRight_12(t_89);
      }break;
     }break;
    }break;
   }break;
  }
  return t_89;}();};
 function otherColor_15(c_90){
  return function(){
  switch(c_90[0]){
   case "Black":
   return Red_0;
   case "Red":
   return Black_1;
  }
  throw "Non-exhaustive pattern match in case";}();};
 function color_flip_16(t_91){
  return function(){
  switch(t_91[0]){
   case "RBNode":
   switch(t_91[4][0]){
    case "RBNode":
    switch(t_91[5][0]){
     case "RBNode":
     return RBNode_2(otherColor_15(t_91[1]))(t_91[2])(t_91[3])(RBNode_2(otherColor_15(t_91[4][1]))(t_91[4][2])(t_91[4][3])(t_91[4][4])(t_91[4][5]))(RBNode_2(otherColor_15(t_91[5][1]))(t_91[5][2])(t_91[5][3])(t_91[5][4])(t_91[5][5]));
    }break;
   }break;
  }
  return raise_5(Value.str("color_flip called on a RBEmpty or RBNode with a RBEmpty child"));}();};
 function color_flipIfNeeded_17(t_105){
  return function(){
  switch(t_105[0]){
   case "RBNode":
   switch(t_105[4][0]){
    case "RBNode":
    switch(t_105[4][1][0]){
     case "Red":
     switch(t_105[5][0]){
      case "RBNode":
      switch(t_105[5][1][0]){
       case "Red":
       return color_flip_16(t_105);
      }break;
     }break;
    }break;
   }break;
  }
  return t_105;}();};
 function fixUp_18(t_106){
  return color_flipIfNeeded_17(rotateRightIfNeeded_14(rotateLeftIfNeeded_13(t_106)));};
 function ensureBlackRoot_19(t_107){
  return function(){
  switch(t_107[0]){
   case "RBNode":
   switch(t_107[1][0]){
    case "Red":
    return RBNode_2(Black_1)(t_107[2])(t_107[3])(t_107[4])(t_107[5]);
   }break;
  }
  return t_107;}();};
 function insert_20(k_112){
  return function(v_113){
   return function(t_114){
    return function(){
     function ins_115(t_116){
      return function(){
      switch(t_116[0]){
       case "RBEmpty":
       return RBNode_2(Red_0)(k_112)(v_113)(RBEmpty_3)(RBEmpty_3);
       case "RBNode":
       return function(){
        var h_122=function(){
        var case6=compare(k_112)(t_116[2]);
        switch(case6[0]){
         case "EQ":
         return RBNode_2(t_116[1])(t_116[2])(v_113)(t_116[4])(t_116[5]);
         case "GT":
         return RBNode_2(t_116[1])(t_116[2])(t_116[3])(t_116[4])(ins_115(t_116[5]));
         case "LT":
         return RBNode_2(t_116[1])(t_116[2])(t_116[3])(ins_115(t_116[4]))(t_116[5]);
        }
        throw "Non-exhaustive pattern match in case";}();
        return fixUp_18(h_122);}();
      }
      throw "Non-exhaustive pattern match in case";}();};
     return ensureBlackRoot_19(ins_115(t_114));}();};};};
 function singleton_21(k_123){
  return function(v_124){
   return insert_20(k_123)(v_124)(RBEmpty_3);};};
 function isRed_22(t_125){
  return function(){
  switch(t_125[0]){
   case "RBNode":
   switch(t_125[1][0]){
    case "Red":
    return true;
   }break;
  }
  return false;}();};
 function isRedLeft_23(t_126){
  return function(){
  switch(t_126[0]){
   case "RBNode":
   switch(t_126[4][0]){
    case "RBNode":
    switch(t_126[4][1][0]){
     case "Red":
     return true;
    }break;
   }break;
  }
  return false;}();};
 function isRedLeftLeft_24(t_127){
  return function(){
  switch(t_127[0]){
   case "RBNode":
   switch(t_127[4][0]){
    case "RBNode":
    switch(t_127[4][4][0]){
     case "RBNode":
     switch(t_127[4][4][1][0]){
      case "Red":
      return true;
     }break;
    }break;
   }break;
  }
  return false;}();};
 function isRedRight_25(t_128){
  return function(){
  switch(t_128[0]){
   case "RBNode":
   switch(t_128[5][0]){
    case "RBNode":
    switch(t_128[5][1][0]){
     case "Red":
     return true;
    }break;
   }break;
  }
  return false;}();};
 function isRedRightLeft_26(t_129){
  return function(){
  switch(t_129[0]){
   case "RBNode":
   switch(t_129[5][0]){
    case "RBNode":
    switch(t_129[5][4][0]){
     case "RBNode":
     switch(t_129[5][4][1][0]){
      case "Red":
      return true;
     }break;
    }break;
   }break;
  }
  return false;}();};
 function moveRedLeft_27(t_130){
  return function(){
   var t__131=color_flip_16(t_130);
   return function(){
   switch(t__131[0]){
    case "RBNode":
    return function(){
    switch(t__131[5][0]){
     case "RBNode":
     switch(t__131[5][4][0]){
      case "RBNode":
      switch(t__131[5][4][1][0]){
       case "Red":
       return color_flip_16(rotateLeft_11(RBNode_2(t__131[1])(t__131[2])(t__131[3])(t__131[4])(rotateRight_12(t__131[5]))));
      }break;
     }break;
    }
    return t__131;}();
   }
   return t__131;}();}();};
 function moveRedRight_28(t_137){
  return function(){
   var t__138=color_flip_16(t_137);
   return (isRedLeftLeft_24(t__138)?color_flip_16(rotateRight_12(t__138)):t__138);}();};
 function moveRedLeftIfNeeded_29(t_139){
  return ((not(isRedLeft_23(t_139))&&not(isRedLeftLeft_24(t_139)))?moveRedLeft_27(t_139):t_139);};
 function moveRedRightIfNeeded_30(t_140){
  return ((not(isRedRight_25(t_140))&&not(isRedRightLeft_26(t_140)))?moveRedRight_28(t_140):t_140);};
 function deleteMin_31(t_141){
  return function(){
   function del_142(t_143){
    return function(){
    switch(t_143[0]){
     case "RBNode":
     switch(t_143[4][0]){
      case "RBEmpty":
      return RBEmpty_3;
     }break;
    }
    return function(){
     var t__144=moveRedLeftIfNeeded_29(t_143);
     return function(){
     switch(t__144[0]){
      case "RBEmpty":
      return RBEmpty_3;
      case "RBNode":
      return fixUp_18(RBNode_2(t__144[1])(t__144[2])(t__144[3])(del_142(t__144[4]))(t__144[5]));
     }
     throw "Non-exhaustive pattern match in case";}();}();}();};
   return ensureBlackRoot_19(del_142(t_141));}();};
 function remove_32(k_150){
  return function(t_151){
   return function(){
    function eq_and_noRightNode_152(t_153){
     return function(){
     switch(t_153[0]){
      case "RBNode":
      switch(t_153[5][0]){
       case "RBEmpty":
       return eq(k_150,t_153[2]);
      }break;
     }
     return false;}();};
    return function(){
     function eq_155(t_156){
      return function(){
      switch(t_156[0]){
       case "RBNode":
       return eq(k_150,t_156[2]);
      }
      return false;}();};
     return function(){
      function delLT_158(t_159){
       return function(){
        var t__160=moveRedLeftIfNeeded_29(t_159);
        return function(){
        switch(t__160[0]){
         case "RBEmpty":
         return raise_5(Value.str("delLT on RBEmpty"));
         case "RBNode":
         return fixUp_18(RBNode_2(t__160[1])(t__160[2])(t__160[3])(del(t__160[4]))(t__160[5]));
        }
        throw "Non-exhaustive pattern match in case";}();}();};
      return function(){
       function delEQ_166(t_167){
        return function(){
        switch(t_167[0]){
         case "RBEmpty":
         return raise_5(Value.str("delEQ called on a RBEmpty"));
         case "RBNode":
         return function(){
          var Tuple2$k_v__171=min_6(t_167[5]);
          var k__172=function(){
          switch(Tuple2$k_v__171[0]){
           case "Tuple2":
           return Tuple2$k_v__171[1];
          }
          throw "Non-exhaustive pattern match in case";}();
          var v__173=function(){
          switch(Tuple2$k_v__171[0]){
           case "Tuple2":
           return Tuple2$k_v__171[2];
          }
          throw "Non-exhaustive pattern match in case";}();
          return fixUp_18(RBNode_2(t_167[1])(k__172)(v__173)(t_167[4])(deleteMin_31(t_167[5])));}();
        }
        throw "Non-exhaustive pattern match in case";}();};
       return function(){
        function delGT_178(t_179){
         return function(){
         switch(t_179[0]){
          case "RBEmpty":
          return raise_5(Value.str("delGT called on a RBEmpty"));
          case "RBNode":
          return fixUp_18(RBNode_2(t_179[1])(t_179[2])(t_179[3])(t_179[4])(del(t_179[5])));
         }
         throw "Non-exhaustive pattern match in case";}();};
        return function(){
         function del_185(t_186){
          return function(){
          switch(t_186[0]){
           case "RBEmpty":
           return RBEmpty_3;
           case "RBNode":
           return ((compare(k_150)(t_186[2])[0] === 'LT')?delLT_158(t_186):function(){
            var t__188=(isRedLeft_23(t_186)?rotateRight_12(t_186):t_186);
            return (eq_and_noRightNode_152(t__188)?RBEmpty_3:function(){
             var t_189=moveRedRightIfNeeded_30(t_189);
             return (eq_155(t_189)?delEQ_166(t_189):delGT_178(t_189));}());}());
          }
          throw "Non-exhaustive pattern match in case";}();};
         return ensureBlackRoot_19(del_185(t_151));}();}();}();}();}();}();};};
 function map_33(f_190){
  return function(t_191){
   return function(){
   switch(t_191[0]){
    case "RBEmpty":
    return RBEmpty_3;
    case "RBNode":
    return RBNode_2(t_191[1])(t_191[2])(f_190(t_191[3]))(map_33(f_190)(t_191[4]))(map_33(f_190)(t_191[5]));
   }
   throw "Non-exhaustive pattern match in case";}();};};
 function foldl_34(f_197){
  return function(acc_198){
   return function(t_199){
    return function(){
    switch(t_199[0]){
     case "RBEmpty":
     return acc_198;
     case "RBNode":
     return foldl_34(f_197)(f_197(t_199[2])(t_199[3])(foldl_34(f_197)(acc_198)(t_199[4])))(t_199[5]);
    }
    throw "Non-exhaustive pattern match in case";}();};};};
 function foldr_35(f_204){
  return function(acc_205){
   return function(t_206){
    return function(){
    switch(t_206[0]){
     case "RBEmpty":
     return acc_205;
     case "RBNode":
     return foldr_35(f_204)(f_204(t_206[2])(t_206[3])(foldr_35(f_204)(acc_205)(t_206[5])))(t_206[4]);
    }
    throw "Non-exhaustive pattern match in case";}();};};};
 function union_36(t1_211){
  return function(t2_212){
   return foldl_34(insert_20)(t2_212)(t1_211);};};
 function intersect_37(t1_213){
  return function(t2_214){
   return foldl_34(function(k_215){
    return function(v_216){
     return function(t_217){
      return (member_10(k_215)(t2_214)?insert_20(k_215)(v_216)(t_217):t_217);};};})(empty_4)(t1_213);};};
 function diff_38(t1_218){
  return function(t2_219){
   return foldl_34(function(k_220){
    return function(__221){
     return function(t_222){
      return remove_32(k_220)(t_222);};};})(t1_218)(t2_219);};};
 function keys_39(t_223){
  return foldl_34(function(k_224){
   return function(__225){
    return function(acc_226){
     return ["Cons",k_224,acc_226];};};})(["Nil"])(t_223);};
 function values_40(t_227){
  return foldl_34(function(__228){
   return function(x_229){
    return function(y_230){
     return ["Cons",x_229,y_230];};};})(["Nil"])(t_227);};
 function toList_41(t_231){
  return foldl_34(function(k_232){
   return function(v_233){
    return function(acc_234){
     return ["Cons",["Tuple2",k_232,v_233],acc_234];};};})(["Nil"])(t_231);};
 function fromList_42(assocs_235){
  return List.foldl(uncurry(insert_20))(empty_4)(assocs_235);};
 return {empty:empty_4,lookup:lookup_7,findWithDefault:findWithDefault_8,find:find_9,member:member_10,insert:insert_20,singleton:singleton_21,remove:remove_32,map:map_33,foldl:foldl_34,foldr:foldr_35,union:union_36,intersect:intersect_37,diff:diff_38,keys:keys_39,values:values_40,toList:toList_41,fromList:fromList_42};}();
Elm.main=function(){
 return Elm.Dict.main;};
} catch (e) {Elm.main=function() {var msg = ('<br/><h2>Your browser may not be supported. Are you using a modern browser?</h2>' + '<br/><span style="color:grey">Runtime Error in Dict module:<br/>' + e + '</span>');document.body.innerHTML = Text.monospace(msg);throw e;};}