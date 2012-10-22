
(function() {

for(var i in Elm) { this[i] = Elm[i]; }
Elm.Set=function(){
 var hiddenVars=[];
 for(var i in Elm.Prelude){
  if (hiddenVars.indexOf(i) >= 0) continue;
  this[i]=Elm.Prelude[i];}
 var empty_0=Dict.empty;
 var remove_3=Dict.remove;
 var member_4=Dict.member;
 var union_5=Dict.union;
 var intersect_6=Dict.intersect;
 var diff_7=Dict.diff;
 var toList_8=Dict.keys;
 var fromList_9=List.foldl(function(k_15){
  return function(t_16){
   return Dict.insert(k_15)(["Tuple0"])(t_16);};})(empty_0);
 function singleton_1(k_13){
  return Dict.singleton(k_13)(["Tuple0"]);};
 function insert_2(k_14){
  return Dict.insert(k_14)(["Tuple0"]);};
 function foldl_10(f_17){
  return Dict.foldl(function(k_18){
   return function(v_19){
    return function(b_20){
     return f_17(k_18)(b_20);};};});};
 function foldr_11(f_21){
  return Dict.foldr(function(k_22){
   return function(v_23){
    return function(b_24){
     return f_21(k_22)(b_24);};};});};
 function map_12(f_25){
  return function(t_26){
   return function(x){
    return fromList_9(List.map(f_25)(x));}(toList_8(t_26));};};
 return {empty:empty_0,singleton:singleton_1,insert:insert_2,remove:remove_3,member:member_4,union:union_5,intersect:intersect_6,diff:diff_7,toList:toList_8,fromList:fromList_9,foldl:foldl_10,foldr:foldr_11,map:map_12};}();
}());