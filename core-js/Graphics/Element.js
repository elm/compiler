var ElmCode = ElmCode || {};
ElmCode.Graphics = ElmCode.Graphics || {};
ElmCode.Graphics.Element = function() {
  var DLeft_0 = ["DLeft"];
  var DRight_1 = ["DRight"];
  var DUp_2 = ["DUp"];
  var DDown_3 = ["DDown"];
  var DIn_4 = ["DIn"];
  var DOut_5 = ["DOut"];
  function Absolute_12(a1) {
    return["Absolute", a1]
  }
  function Relative_13(a1) {
    return["Relative", a1]
  }
  var Near_14 = ["Near"];
  var Mid_15 = ["Mid"];
  var Far_16 = ["Far"];
  function Position_17(a1) {
    return function(a2) {
      return["Position", a1, a2]
    }
  }
  function PositionTL_18(a1) {
    return function(a2) {
      return["PositionTL", a1, a2]
    }
  }
  function PositionTR_19(a1) {
    return function(a2) {
      return["PositionTR", a1, a2]
    }
  }
  function PositionBL_20(a1) {
    return function(a2) {
      return["PositionBL", a1, a2]
    }
  }
  function PositionBR_21(a1) {
    return function(a2) {
      return["PositionBR", a1, a2]
    }
  }
  function Element_37(a1) {
    return function(a2) {
      return function(a3) {
        return function(a4) {
          return function(a5) {
            return function(a6) {
              return["Element", a1, a2, a3, a4, a5, a6]
            }
          }
        }
      }
    }
  }
  function EText_39(a1) {
    return function(a2) {
	return["EText", a1, a2]
    }
  }
  function EImage_40(a1) {
    return["EImage", Foreign.JavaScript.castStringToJSString(a1)]
  }
  function EVideo_41(a1) {
    return["EVideo", a1]
  }
  function EFittedImage_42(a1) {
    return["EFittedImage", a1]
  }
  function EFlow_43(a1) {
    return function(a2) {
      return["EFlow", a1, Foreign.JavaScript.castListToJSArray(a2)]
    }
  }
  function ECollage_44(a1) {
    return function(a2) {
      return function(a3) {
	  return["ECollage", a1, a2, Value.groupForms(a3)]
      }
    }
  }
  var EEmpty_45 = ["EEmpty"];
  function EContainer_46(a1) {
    return function(a2) {
      return["EContainer", a1, a2]
    }
  }
  var Solid_68 = ["Solid"];
  var Dotted_69 = ["Dotted"];
  var Dashed_70 = ["Dashed"];
  function Custom_71(a1) {
    return["Custom", a1]
  }
  var Filled_72 = ["Filled"];
  var Outlined_73 = ["Outlined"];
  function CustomOutline_74(a1) {
    return["CustomOutline", a1]
  }
  function Line_75(a1) {
    return["Line", a1]
  }
  function Shape_78(a1) {
    return function(a2) {
      return["Shape", a1, a2]
    }
  }
  function Form_84(a1) {
    return function(a2) {
      return function(a3) {
        return function(a4) {
          return["Form", a1, a2, a3, a4]
        }
      }
    }
  }
  function FLine_85(a1) {
    return function(a2) {
      return function(a3) {
        return["FLine", a1, a2, a3]
      }
    }
  }
  function FShape_86(a1) {
    return function(a2) {
      return function(a3) {
        return["FShape", a1, a2, a3]
      }
    }
  }
  function FImage_87(a1) {
    return function(a2) {
      return function(a3) {
          return["FImage", a1, a2, Foreign.JavaScript.castStringToJSString(a3)]
      }
    }
  }
  function FElement_88(a1) {
      return["FElement", a1]
  }
  var left_6 = DLeft_0;
  var right_7 = DRight_1;
  var down_8 = DDown_3;
  var up_9 = DUp_2;
  var inward_10 = DIn_4;
  var outward_11 = DOut_5;
  var topLeft_22 = Position_17(Near_14)(Near_14);
  var topRight_23 = Position_17(Far_16)(Near_14);
  var bottomLeft_24 = Position_17(Near_14)(Far_16);
  var bottomRight_25 = Position_17(Far_16)(Far_16);
  var midLeft_26 = Position_17(Near_14)(Mid_15);
  var midRight_27 = Position_17(Far_16)(Mid_15);
  var midTop_28 = Position_17(Mid_15)(Near_14);
  var midBottom_29 = Position_17(Mid_15)(Far_16);
  var middle_30 = Position_17(Mid_15)(Mid_15);
  function middleAt(a1) {
    return function(a2) {
      return["PositionAt", a1, a2]
    }
  }
  var topLeftAt_31 = PositionTL_18;
  var topRightAt_32 = PositionTR_19;
  var bottomLeftAt_33 = PositionBL_20;
  var bottomRightAt_34 = PositionBR_21;
  var absolute_35 = Absolute_12;
  var relative_36 = Relative_13;
  function newElement_38(a) { return Element_37(Guid.guid())(a); }
  var line_76 = Line_75;
  var polygon_79 = Shape_78;
  function sprite_96(src) {
    return function(w) {
      return function(h) {
        return function(pos) {
          return Form_84(0)(1)(pos)(FImage_87(w)(h)(src))
	}
      }
    }
  }
  function toForm_97(pos) {
    return function(e) {
      return Form_84(0)(1)(pos)(FElement_88(e))
    }
  }
  function width_47(w__101) {
    return function(Element$_ewhoc_102) {
      return function(case7) {
        var case0 = case7;
        switch(case0[0]) {
          case "Element":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4], case5 = case0[5], case6 = case0[6];
            return function(case12) {
              var case8 = case12;
              switch(case8[0]) {
                case "EImage":
                case "EVideo":
                  return newElement_38(case2)(w__101)(case4 * w__101 / case3)(case5)(case6);
                case "EText":
                  var p = Value.getTextSize(w__101,case4,case8[2]);
                  return newElement_38(case2)(w__101)(p[1])(case5)(case6);
              }
              return newElement_38(case2)(w__101)(case4)(case5)(case6)
            }(case2)
        }
        throw"Non-exhaustive pattern match in case";
      }(Element$_ewhoc_102)
    }
  }
  function height_48(h__108) {
    return function(Element$_ewhoc_109) {
      return function(case7) {
        var case0 = case7;
        switch(case0[0]) {
          case "Element":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4], case5 = case0[5], case6 = case0[6];
            return function(case12) {
              var case8 = case12;
              switch(case8[0]) {
                case "EImage":
                case "EVideo":
                  return newElement_38(case2)(case3 * h__108 / case4)(h__108)(case5)(case6);
              }
              return newElement_38(case2)(case3)(h__108)(case5)(case6)
            }(case2)
        }
        throw"Non-exhaustive pattern match in case";
      }(Element$_ewhoc_109)
    }
  }
  function size_49(w__115) {
    return function(h__116) {
      return function(Element$_ewhoc_117) {
        return function(case7) {
          var case0 = case7;
          switch(case0[0]) {
            case "Element":
              var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4], case5 = case0[5], case6 = case0[6];
              return newElement_38(case2)(w__115)(h__116)(case5)(case6)
          }
          throw"Non-exhaustive pattern match in case";
        }(Element$_ewhoc_117)
      }
    }
  }
  function opacity_50(o__123) {
    return function(Element$_ewhoc_124) {
      return function(case7) {
        var case0 = case7;
        switch(case0[0]) {
          case "Element":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4], case5 = case0[5], case6 = case0[6];
            return newElement_38(case2)(case3)(case4)(o__123)(case6)
        }
        throw"Non-exhaustive pattern match in case";
      }(Element$_ewhoc_124)
    }
  }
  function color_51(c__130) {
    return function(Element$_ewhoc_131) {
      return function(case7) {
        var case0 = case7;
        switch(case0[0]) {
          case "Element":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4], case5 = case0[5], case6 = case0[6];
            return newElement_38(case2)(case3)(case4)(case5)(Just(c__130))
        }
        throw"Non-exhaustive pattern match in case";
      }(Element$_ewhoc_131)
    }
  }
  function widthOf_52(e)  { return ~~e[3]; }
  function heightOf_53(e) { return ~~e[4]; }
  function sizeOf_54(e)   { return["Tuple2", ~~e[3], ~~e[4]] }
  function text_56(txt) {
    var p = Value.getTextSize(-1,-1,txt);
    return newElement_38(EText_39("left")(txt))(p[0])(p[1])(1)(Nothing)
  }
  function plainText(str) {
    var txt = Value.toText(str);
    var p = Value.getTextSize(-1,-1,txt);
    return newElement_38(EText_39("left")(txt))(p[0])(p[1])(1)(Nothing)
  }
  function asText(v) {
    var txt = Value.show(v);
    var p = Value.getTextSize(-1,-1,txt);
    return newElement_38(EText_39("left")(txt))(p[0])(p[1])(1)(Nothing)
  }
  function centeredText(txt) {
    var p = Value.getTextSize(-1,-1,txt);
    return newElement_38(EText_39("center")(txt))(p[0])(p[1])(1)(Nothing)
  }
  function justifiedText(txt) {
    var p = Value.getTextSize(-1,-1,txt);
    return newElement_38(EText_39("justify")(txt))(p[0])(p[1])(1)(Nothing)
  }
  function rightedText(txt) {
    var p = Value.getTextSize(-1,-1,txt);
    return newElement_38(EText_39("right")(txt))(p[0])(p[1])(1)(Nothing)
  }
  function image_57(w) {
    return function(h) {
      return function(src) {
	return newElement_38(EImage_40(src))(w)(h)(1)(Nothing)
      }
    }
  }
  function video_58(w) {
    return function(h) {
      return function(src) {
        return newElement_38(EVideo_41(src))(w)(h)(1)(Nothing)
      }
    }
  }
  function fittedImage_59(w_147) {
    return function(h_148) {
      return function(s_149) {
        return newElement_38(EFittedImage_42(s_149))(w_147)(h_148)(1)(Nothing)
      }
    }
  }
  function flow_60(dir_150) {
    return function(es_151) {
      return function() {
        var w_152 = function() {
          var ws_154 = map(widthOf_52)(es_151);
          return function(case1) {
            var case0 = case1;
            switch(case0[0]) {
              case "DLeft":
                return sum(ws_154);
              case "DRight":
                return sum(ws_154)
            }
            return maximum(ws_154)
          }(dir_150)
        }();
        var h_153 = function() {
          var hs_155 = map(heightOf_53)(es_151);
          return function(case3) {
            var case2 = case3;
            switch(case2[0]) {
              case "DDown":
                return sum(hs_155);
              case "DUp":
                return sum(hs_155)
            }
            return maximum(hs_155)
          }(dir_150)
        }();
        return newElement_38(EFlow_43(dir_150)(es_151))(w_152)(h_153)(1)(Nothing)
      }()
    }
  }
  function above_61(e1_156) {
    return function(e2_157) {
      return newElement_38(EFlow_43(DDown_3)(["Cons", e1_156, ["Cons", e2_157, ["Nil"]]]))(max(widthOf_52(e1_156))(widthOf_52(e2_157)))(heightOf_53(e1_156) + heightOf_53(e2_157))(1)(Nothing)
    }
  }
  function below_62(e1_158) {
    return function(e2_159) {
      return newElement_38(EFlow_43(DDown_3)(["Cons", e2_159, ["Cons", e1_158, ["Nil"]]]))(max(widthOf_52(e1_158))(widthOf_52(e2_159)))(heightOf_53(e1_158) + heightOf_53(e2_159))(1)(Nothing)
    }
  }
  function beside_63(e1_160) {
    return function(e2_161) {
      return newElement_38(EFlow_43(DRight_1)(["Cons", e1_160, ["Cons", e2_161, ["Nil"]]]))(widthOf_52(e1_160) + widthOf_52(e2_161))(max(heightOf_53(e1_160))(heightOf_53(e2_161)))(1)(Nothing)
    }
  }
  function layers_64(es_162) {
    return newElement_38(EFlow_43(DOut_5)(es_162))(maximum(map(widthOf_52)(es_162)))(maximum(map(heightOf_53)(es_162)))(1)(Nothing)
  }
  function collage_65(w_163) {
    return function(h_164) {
      return function(forms_165) {
        return newElement_38(ECollage_44(w_163)(h_164)(forms_165))(w_163)(h_164)(1)(Nothing)
      }
    }
  }
  function spacer_66(w_166) {
    return function(h_167) {
      return newElement_38(EEmpty_45)(w_166)(h_167)(1)(Nothing)
    }
  }
  function container_67(w_169) {
    return function(h_170) {
      return function(pos_168) {
        return function(e_171) {
          return newElement_38(EContainer_46(pos_168)(e_171))(w_169)(h_170)(1)(Nothing)
        }
      }
    }
  }
  function segment_77(p1_172) {
    return function(p2_173) {
      return Line_75(["Cons", p1_172, ["Cons", p2_173, ["Nil"]]])
    }
  }
  function rect_80(w_174) {
    return function(h_175) {
      return function(pos_176) {
        return Shape_78(["Cons", ["Tuple2", 0 - w_174 / 2, 0 - h_175 / 2], ["Cons", ["Tuple2", 0 - w_174 / 2, h_175 / 2], ["Cons", ["Tuple2", w_174 / 2, h_175 / 2], ["Cons", ["Tuple2", w_174 / 2, 0 - h_175 / 2], ["Nil"]]]]])(pos_176)
      }
    }
  }
  function oval_81(w_177) {
    return function(h_178) {
      return function(pos_179) {
        return function() {
          var n_180 = 50;
          return function() {
            function f_181(i_182) {
              return["Tuple2", w_177 / 2 * cos(2 * (pi / n_180) * i_182), h_178 / 2 * sin(2 * (pi / n_180) * i_182)]
            }
            return Shape_78(map(f_181)(function() {
              var lo = 0;
              var hi = n_180 - 1;
              var lst = ["Nil"];
              if(lo <= hi) {
                do {
                  var lst = ["Cons", hi, lst]
                }while(hi-- > lo)
              }
              return lst
            }()))(pos_179)
          }()
        }()
      }
    }
  }
  function circle_82(r_183) {
    return oval_81(2 * r_183)(2 * r_183)
  }
  function ngon_83(n_184) {
    return function(r_185) {
      return function(pos_186) {
        return function() {
          var m_187 = toFloat(n_184);
          return function() {
            function f_188(i_189) {
              return["Tuple2", r_185 * cos(2 * (pi / m_187) * i_189), r_185 * sin(2 * (pi / m_187) * i_189)]
            }
            return Shape_78(map(f_188)(function() {
              var lo = 0;
              var hi = n_184 - 1;
              var lst = ["Nil"];
              if(lo <= hi) {
                do {
                  var lst = ["Cons", hi, lst]
                }while(hi-- > lo)
              }
              return lst
            }()))(pos_186)
          }()
        }()
      }
    }
  }
  function solid_89(clr_190) {
    return function(ln_191) {
      return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Solid_68)(clr_190)(ln_191))
    }
  }
  function dotted_90(clr_192) {
    return function(ln_193) {
      return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Dotted_69)(clr_192)(ln_193))
    }
  }
  function dashed_91(clr_194) {
    return function(ln_195) {
      return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Dashed_70)(clr_194)(ln_195))
    }
  }
  function customLine_92(pattern_196) {
    return function(clr_197) {
      return function(ln_198) {
        return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Custom_71(pattern_196))(clr_197)(ln_198))
      }
    }
  }
  function filled_93(clr) {
    return function(shape) {
      return Form_84(0)(1)(shape[2])(FShape_86(Filled_72)(clr)(shape));
    }
  }
  function outlined_94(clr) {
    return function(shape) {
      return Form_84(0)(1)(shape[2])(FShape_86(Outlined_73)(clr)(shape));
    }
  }
  function customOutline_95(pattern) {
    return function(clr) {
      return function(shape) {
	return Form_84(0)(1)(shape[2])(FShape_86(["CustomOutline",pattern])(clr)(shape));
      }
    }
  }
  function textured(src) {
    return function(shape) {
      return Form_84(0)(1)(shape[2])(FShape_86(["Textured",src])(null)(shape));
    }
  }
  function rotate_98(t_212) {
    return function(Form$thetascaleposform_213) {
      return function(case5) {
        var case0 = case5;
        switch(case0[0]) {
          case "Form":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4];
            return Form_84(t_212 + case1)(case2)(case3)(case4)
        }
        throw"Non-exhaustive pattern match in case";
      }(Form$thetascaleposform_213)
    }
  }
  function scale_99(s_218) {
    return function(Form$thetascaleposform_219) {
      return function(case5) {
        var case0 = case5;
        switch(case0[0]) {
          case "Form":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4];
            return Form_84(case1)(s_218 * case2)(case3)(case4)
        }
        throw"Non-exhaustive pattern match in case";
      }(Form$thetascaleposform_219)
    }
  }
  function move_100(x_224) {
    return function(y_225) {
      return function(Form$thetascaleTuple2$pxpyform_226) {
        return function(case7) {
          var case0 = case7;
          switch(case0[0]) {
            case "Form":
              var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4];
              switch(case3[0]) {
                case "Tuple2":
                  var case5 = case3[1], case6 = case3[2];
                  return Form_84(case1)(case2)(["Tuple2", x_224 + case5, y_225 + case6])(case4)
              }
              break
          }
          throw"Non-exhaustive pattern match in case";
        }(Form$thetascaleTuple2$pxpyform_226)
      }
    }
  }
  return{left:left_6, right:right_7, down:down_8, up:up_9, inward:inward_10, outward:outward_11, topLeft:topLeft_22, topRight:topRight_23, bottomLeft:bottomLeft_24, bottomRight:bottomRight_25, midLeft:midLeft_26, midRight:midRight_27, midTop:midTop_28, midBottom:midBottom_29, middle:middle_30, middleAt:middleAt, topLeftAt:topLeftAt_31, topRightAt:topRightAt_32, bottomLeftAt:bottomLeftAt_33, bottomRightAt:bottomRightAt_34, absolute:absolute_35, relative:relative_36, width:width_47, height:height_48, size:size_49, opacity:opacity_50, 
	 color:color_51, widthOf:widthOf_52, heightOf:heightOf_53, sizeOf:sizeOf_54, text:text_56, plainText:plainText, centeredText:centeredText, justifiedText:justifiedText, rightedText:rightedText, image:image_57, video:video_58, fittedImage:fittedImage_59, flow:flow_60, above:above_61, below:below_62, beside:beside_63, layers:layers_64, collage:collage_65, spacer:spacer_66, container:container_67, line:line_76, segment:segment_77, polygon:polygon_79, rect:rect_80, oval:oval_81, circle:circle_82, ngon:ngon_83, solid:solid_89, dotted:dotted_90, dashed:dashed_91, customLine:customLine_92, filled:filled_93, 
	 outlined:outlined_94, customOutline:customOutline_95, textured:textured, sprite:sprite_96, toForm:toForm_97, rotate:rotate_98, scale:scale_99, move:move_100}
}();