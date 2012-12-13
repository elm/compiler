
Elm.Dict=function(){
 var compare = Elm.Prelude.compare;
 var uncurry = Elm.Prelude.uncurry;
 var Nothing = Elm.Prelude.Nothing;
 var Just = Elm.Prelude.Just;
 var not = Elm.Prelude.not;
 var eq = Elm.Prelude.eq;
 var isJust=Elm.Maybe.isJust;
 var Red_0=['Red'];
 var Black_1=['Black'];
 function RBNode_2(a1){
  return function(a2){
   return function(a3){
    return function(a4){
     return function(a5){
      return ['RBNode',a1,a2,a3,a4,a5];};};};};}
 var RBEmpty_3=['RBEmpty'];
 function min_6(t_42){
  return function(){
  switch(t_42[0]){
   case 'RBEmpty':
   throw '(min RBEmpty) is not defined';
   case 'RBNode':
   switch(t_42[4][0]){
    case 'RBEmpty':
    return ['Tuple2',t_42[2],t_42[3]];
   }
   return min_6(t_42[4]);
  }
  throw "Non-exhaustive pattern match in case";}();}
 function lookup_7(k_46){
  return function(t_47){
   return function(){
   switch(t_47[0]){
    case 'RBEmpty':
    return Nothing;
    case 'RBNode':
    return function(){
    var case12=compare(k_46)(t_47[2]);
    switch(case12[0]){
     case 'EQ':
     return Just(t_47[3]);
     case 'GT':
     return lookup_7(k_46)(t_47[5]);
     case 'LT':
     return lookup_7(k_46)(t_47[4]);
    }
    throw "Non-exhaustive pattern match in case";}();
   }
   throw "Non-exhaustive pattern match in case";}();};}
 function findWithDefault_8(base_52){
  return function(k_53){
   return function(t_54){
    return function(){
    switch(t_54[0]){
     case 'RBEmpty':
     return base_52;
     case 'RBNode':
     return function(){
     var case19=compare(k_53)(t_54[2]);
     switch(case19[0]){
      case 'EQ':
      return t_54[3];
      case 'GT':
      return findWithDefault_8(base_52)(k_53)(t_54[5]);
      case 'LT':
      return findWithDefault_8(base_52)(k_53)(t_54[4]);
     }
     throw "Non-exhaustive pattern match in case";}();
    }
    throw "Non-exhaustive pattern match in case";}();};};}
 function member_9(k_59){
  return function(t_60){
   return isJust(lookup_7(k_59)(t_60));};}
 function rotateLeft_10(t_61){
  return function(){
  switch(t_61[0]){
   case 'RBNode':
   switch(t_61[5][0]){
    case 'RBNode':
    return RBNode_2(t_61[1])(t_61[5][2])(t_61[5][3])(RBNode_2(Red_0)(t_61[2])(t_61[3])(t_61[4])(t_61[5][4]))(t_61[5][5]);
   }break;
  }
  throw 'rotateLeft of a node without enough children';}();}
 function rotateRight_11(t_71){
  return function(){
  switch(t_71[0]){
   case 'RBNode':
   switch(t_71[4][0]){
    case 'RBNode':
    return RBNode_2(t_71[1])(t_71[4][2])(t_71[4][3])(t_71[4][4])(RBNode_2(Red_0)(t_71[2])(t_71[3])(t_71[4][5])(t_71[5]));
   }break;
  }
  throw 'rotateRight of a node without enough children';}();}
 function rotateLeftIfNeeded_12(t_81){
  return function(){
  switch(t_81[0]){
   case 'RBNode':
   switch(t_81[5][0]){
    case 'RBNode':
    switch(t_81[5][1][0]){
     case 'Red':
     return rotateLeft_10(t_81);
    }break;
   }break;
  }
  return t_81;}();}
 function rotateRightIfNeeded_13(t_82){
  return function(){
  switch(t_82[0]){
   case 'RBNode':
   switch(t_82[4][0]){
    case 'RBNode':
    switch(t_82[4][1][0]){
     case 'Red':
     switch(t_82[4][4][0]){
      case 'RBNode':
      switch(t_82[4][4][1][0]){
       case 'Red':
       return rotateRight_11(t_82);
      }break;
     }break;
    }break;
   }break;
  }
  return t_82;}();}
 function otherColor_14(c_83){
  return function(){
  switch(c_83[0]){
   case 'Black':
   return Red_0;
   case 'Red':
   return Black_1;
  }
  throw "Non-exhaustive pattern match in case";}();}
 function color_flip_15(t_84){
  return function(){
  switch(t_84[0]){
   case 'RBNode':
   switch(t_84[4][0]){
    case 'RBNode':
    switch(t_84[5][0]){
     case 'RBNode':
     return RBNode_2(otherColor_14(t_84[1]))(t_84[2])(t_84[3])(RBNode_2(otherColor_14(t_84[4][1]))(t_84[4][2])(t_84[4][3])(t_84[4][4])(t_84[4][5]))(RBNode_2(otherColor_14(t_84[5][1]))(t_84[5][2])(t_84[5][3])(t_84[5][4])(t_84[5][5]));
    }break;
   }break;
  }
  throw 'color_flip called on a RBEmpty or RBNode with a RBEmpty child';}();}
 function color_flipIfNeeded_16(t_98){
  return function(){
  switch(t_98[0]){
   case 'RBNode':
   switch(t_98[4][0]){
    case 'RBNode':
    switch(t_98[4][1][0]){
     case 'Red':
     switch(t_98[5][0]){
      case 'RBNode':
      switch(t_98[5][1][0]){
       case 'Red':
       return color_flip_15(t_98);
      }break;
     }break;
    }break;
   }break;
  }
  return t_98;}();}
 function fixUp_17(t_99){
  return color_flipIfNeeded_16(rotateRightIfNeeded_13(rotateLeftIfNeeded_12(t_99)));}
 function ensureBlackRoot_18(t_100){
  return function(){
  switch(t_100[0]){
   case 'RBNode':
   switch(t_100[1][0]){
    case 'Red':
    return RBNode_2(Black_1)(t_100[2])(t_100[3])(t_100[4])(t_100[5]);
   }break;
  }
  return t_100;}();}
 function insert_19(k_105){
  return function(v_106){
   return function(t_107){
    return function(){
     function ins_108(t_109){
      return function(){
      switch(t_109[0]){
       case 'RBEmpty':
       return RBNode_2(Red_0)(k_105)(v_106)(RBEmpty_3)(RBEmpty_3);
       case 'RBNode':
       return function(){
        var h_115=function(){
        var case114=compare(k_105)(t_109[2]);
        switch(case114[0]){
         case 'EQ':
         return RBNode_2(t_109[1])(t_109[2])(v_106)(t_109[4])(t_109[5]);
         case 'GT':
         return RBNode_2(t_109[1])(t_109[2])(t_109[3])(t_109[4])(ins_108(t_109[5]));
         case 'LT':
         return RBNode_2(t_109[1])(t_109[2])(t_109[3])(ins_108(t_109[4]))(t_109[5]);
        }
        throw "Non-exhaustive pattern match in case";}();
        return fixUp_17(h_115);}();
      }
      throw "Non-exhaustive pattern match in case";}();}
     return ensureBlackRoot_18(ins_108(t_107));}();};};}
 function singleton_20(k_116){
  return function(v_117){
   return insert_19(k_116)(v_117)(RBEmpty_3);};}
 function isRed_21(t_118){
  return function(){
  switch(t_118[0]){
   case 'RBNode':
   switch(t_118[1][0]){
    case 'Red':
    return true;
   }break;
  }
  return false;}();}
 function isRedLeft_22(t_119){
  return function(){
  switch(t_119[0]){
   case 'RBNode':
   switch(t_119[4][0]){
    case 'RBNode':
    switch(t_119[4][1][0]){
     case 'Red':
     return true;
    }break;
   }break;
  }
  return false;}();}
 function isRedLeftLeft_23(t_120){
  return function(){
  switch(t_120[0]){
   case 'RBNode':
   switch(t_120[4][0]){
    case 'RBNode':
    switch(t_120[4][4][0]){
     case 'RBNode':
     switch(t_120[4][4][1][0]){
      case 'Red':
      return true;
     }break;
    }break;
   }break;
  }
  return false;}();}
 function isRedRight_24(t_121){
  return function(){
  switch(t_121[0]){
   case 'RBNode':
   switch(t_121[5][0]){
    case 'RBNode':
    switch(t_121[5][1][0]){
     case 'Red':
     return true;
    }break;
   }break;
  }
  return false;}();}
 function isRedRightLeft_25(t_122){
  return function(){
  switch(t_122[0]){
   case 'RBNode':
   switch(t_122[5][0]){
    case 'RBNode':
    switch(t_122[5][4][0]){
     case 'RBNode':
     switch(t_122[5][4][1][0]){
      case 'Red':
      return true;
     }break;
    }break;
   }break;
  }
  return false;}();}
 function moveRedLeft_26(t_123){
  return function(){
   var t__124=color_flip_15(t_123);
   return function(){
   switch(t__124[0]){
    case 'RBNode':
    return function(){
    switch(t__124[5][0]){
     case 'RBNode':
     switch(t__124[5][4][0]){
      case 'RBNode':
      switch(t__124[5][4][1][0]){
       case 'Red':
       return color_flip_15(rotateLeft_10(RBNode_2(t__124[1])(t__124[2])(t__124[3])(t__124[4])(rotateRight_11(t__124[5]))));
      }break;
     }break;
    }
    return t__124;}();
   }
   return t__124;}();}();}
 function moveRedRight_27(t_130){
  return function(){
   var t__131=color_flip_15(t_130);
   return (isRedLeftLeft_23(t__131)?color_flip_15(rotateRight_11(t__131)):t__131);}();}
 function moveRedLeftIfNeeded_28(t_132){
  return ((not(isRedLeft_22(t_132))&&not(isRedLeftLeft_23(t_132)))?moveRedLeft_26(t_132):t_132);}
 function moveRedRightIfNeeded_29(t_133){
  return ((not(isRedRight_24(t_133))&&not(isRedRightLeft_25(t_133)))?moveRedRight_27(t_133):t_133);}
 function deleteMin_30(t_134){
  return function(){
   function del_135(t_136){
    return function(){
    switch(t_136[0]){
     case 'RBNode':
     switch(t_136[4][0]){
      case 'RBEmpty':
      return RBEmpty_3;
     }break;
    }
    return function(){
    var case198=moveRedLeftIfNeeded_28(t_136);
    switch(case198[0]){
     case 'RBEmpty':
     return RBEmpty_3;
     case 'RBNode':
     return fixUp_17(RBNode_2(case198[1])(case198[2])(case198[3])(del_135(case198[4]))(case198[5]));
    }
    throw "Non-exhaustive pattern match in case";}();}();}
   return ensureBlackRoot_18(del_135(t_134));}();}
 function remove_31(k_142){
  return function(t_143){
   return function(){
    function eq_and_noRightNode_144(t_150){
     return function(){
     switch(t_150[0]){
      case 'RBNode':
      switch(t_150[5][0]){
       case 'RBEmpty':
       return eq(k_142,t_150[2]);
      }break;
     }
     return false;}();}
    function eq_145(t_152){
     return function(){
     switch(t_152[0]){
      case 'RBNode':
      return eq(k_142,t_152[2]);
     }
     return false;}();}
    function delLT_146(t_154){
     return function(){
     var case216=moveRedLeftIfNeeded_28(t_154);
     switch(case216[0]){
      case 'RBEmpty':
      throw 'delLT on RBEmpty';
      case 'RBNode':
      return fixUp_17(RBNode_2(case216[1])(case216[2])(case216[3])(del_149(case216[4]))(case216[5]));
     }
     throw "Non-exhaustive pattern match in case";}();}
    function delEQ_147(t_160){
     return function(){
     switch(t_160[0]){
      case 'RBEmpty':
      throw 'delEQ called on a RBEmpty';
      case 'RBNode':
      return function(){
       var Tuple2$k_v__164=min_6(t_160[5]);
       var k__165=function(){
       switch(Tuple2$k_v__164[0]){
        case 'Tuple2':
        return Tuple2$k_v__164[1];
       }
       throw "Non-exhaustive pattern match in case";}();
       var v__166=function(){
       switch(Tuple2$k_v__164[0]){
        case 'Tuple2':
        return Tuple2$k_v__164[2];
       }
       throw "Non-exhaustive pattern match in case";}();
       return fixUp_17(RBNode_2(t_160[1])(k__165)(v__166)(t_160[4])(deleteMin_30(t_160[5])));}();
     }
     throw "Non-exhaustive pattern match in case";}();}
    function delGT_148(t_171){
     return function(){
     switch(t_171[0]){
      case 'RBEmpty':
      throw 'delGT called on a RBEmpty';
      case 'RBNode':
      return fixUp_17(RBNode_2(t_171[1])(t_171[2])(t_171[3])(t_171[4])(del_149(t_171[5])));
     }
     throw "Non-exhaustive pattern match in case";}();}
    function del_149(t_177){
     return function(){
     switch(t_177[0]){
      case 'RBEmpty':
      return RBEmpty_3;
      case 'RBNode':
      return ((compare(k_142)(t_177[2])[0] === 'LT')?delLT_146(t_177):function(){
       var u_179=(isRedLeft_22(t_177)?rotateRight_11(t_177):t_177);
       return (eq_and_noRightNode_144(u_179)?u_179[4]:function(){
        var t__180=moveRedRightIfNeeded_29(t_177);
        return (eq_145(t__180)?delEQ_147(t__180):delGT_148(t__180));}());}());
     }
     throw "Non-exhaustive pattern match in case";}();}
    return (member_9(k_142)(t_143)?ensureBlackRoot_18(del_149(t_143)):t_143);}();};}
 function map_32(f_181){
  return function(t_182){
   return function(){
   switch(t_182[0]){
    case 'RBEmpty':
    return RBEmpty_3;
    case 'RBNode':
    return RBNode_2(t_182[1])(t_182[2])(f_181(t_182[3]))(map_32(f_181)(t_182[4]))(map_32(f_181)(t_182[5]));
   }
   throw "Non-exhaustive pattern match in case";}();};}
 function foldl_33(f_188){
  return function(acc_189){
   return function(t_190){
    return function(){
    switch(t_190[0]){
     case 'RBEmpty':
     return acc_189;
     case 'RBNode':
     return foldl_33(f_188)(f_188(t_190[2])(t_190[3])(foldl_33(f_188)(acc_189)(t_190[4])))(t_190[5]);
    }
    throw "Non-exhaustive pattern match in case";}();};};}
 function foldr_34(f_195){
  return function(acc_196){
   return function(t_197){
    return function(){
    switch(t_197[0]){
     case 'RBEmpty':
     return acc_196;
     case 'RBNode':
     return foldr_34(f_195)(f_195(t_197[2])(t_197[3])(foldr_34(f_195)(acc_196)(t_197[5])))(t_197[4]);
    }
    throw "Non-exhaustive pattern match in case";}();};};}
 function union_35(t1_202){
  return function(t2_203){
   return foldl_33(insert_19)(t2_203)(t1_202);};}
 function intersect_36(t1_204){
  return function(t2_205){
   return foldl_33(function(k_206){
    return function(v_207){
     return function(t_208){
      return (member_9(k_206)(t2_205)?insert_19(k_206)(v_207)(t_208):t_208);};};})(empty_4)(t1_204);};}
 function diff_37(t1_209){
  return function(t2_210){
   return foldl_33(function(k_211){
    return function(v_212){
     return function(t_213){
      return remove_31(k_211)(t_213);};};})(t1_209)(t2_210);};}
 function keys_38(t_214){
  return foldr_34(function(k_215){
   return function(v_216){
    return function(acc_217){
     return ['Cons',k_215,acc_217];};};})(['Nil'])(t_214);}
 function values_39(t_218){
  return foldr_34(function(k_219){
   return function(v_220){
    return function(acc_221){
     return ['Cons',v_220,acc_221];};};})(['Nil'])(t_218);}
 function toList_40(t_222){
  return foldr_34(function(k_223){
   return function(v_224){
    return function(acc_225){
     return ['Cons',['Tuple2',k_223,v_224],acc_225];};};})(['Nil'])(t_222);}
 function fromList_41(assocs_226){
  return Elm.List.foldl(uncurry(insert_19))(empty_4)(assocs_226);}
 var empty_4=RBEmpty_3;
 return {$op : {},
 empty:empty_4,
 lookup:lookup_7,
 findWithDefault:findWithDefault_8,
 member:member_9,
 insert:insert_19,
 singleton:singleton_20,
 remove:remove_31,
 map:map_32,
 foldl:foldl_33,
 foldr:foldr_34,
 union:union_35,
 intersect:intersect_36,
 diff:diff_37,
 keys:keys_38,
 values:values_39,
 toList:toList_40,
 fromList:fromList_41};}();
