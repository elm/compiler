
ElmRuntime.Render.Element = function() {
'use strict';

var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement, extract = Utils.extract,
    addTransform = Utils.addTransform, removeTransform = Utils.removeTransform,
    fromList = Utils.fromList, eq = Utils.eq;

function setProps(elem, e) {
    var props = elem.props;
    var element = elem.element;
    var width = props.width - (element.adjustWidth || 0);
    var height = props.height - (element.adjustHeight || 0);
    e.style.width  = (width |0) + 'px';
    e.style.height = (height|0) + 'px';
    if (props.opacity !== 1) {
        e.style.opacity = props.opacity;
    }
    if (props.color.ctor === 'Just') {
        e.style.backgroundColor = extract(props.color._0);
    }
    if (props.tag !== '') { e.id = props.tag; }
    if (props.href !== '') {
        var a = newElement('a');
        a.href = props.href;
        a.style.width = '100%';
        a.style.height = '100%';
        a.style.top = 0;
        a.style.left = 0;
        a.style.display = 'block';
        a.style.position = 'absolute';
        e.style.position = 'relative';
        a.style.pointerEvents = 'auto';
        e.appendChild(a);
    }
    if (props.hover.ctor !== '_Tuple0') {
        addHover(e, props.hover);
    }
    if (props.click.ctor !== '_Tuple0') {
        addClick(e, props.click);
    }
    return e;
}

function addClick(e, handler) {
    e.style.pointerEvents = 'auto';
    e.elm_click_handler = handler;
    function trigger(ev) {
        e.elm_click_handler(Utils.Tuple0);
        ev.stopPropagation();
    }
    e.elm_click_trigger = trigger;
    e.addEventListener('click', trigger);
}

function removeClick(e, handler) {
    if (e.elm_click_trigger) {
        e.removeEventListener('click', e.elm_click_trigger);
    }
}

function addHover(e, handler) {
    e.style.pointerEvents = 'auto';
    e.elm_hover_handler = handler;
    e.elm_hover_count = 0;

    function over(evt) {
        if (e.elm_hover_count++ > 0) return;
        e.elm_hover_handler(true);
        evt.stopPropagation();
    }
    function out(evt) {
        if (e.contains(evt.toElement || evt.relatedTarget)) return;
        e.elm_hover_count = 0;
        e.elm_hover_handler(false);
        evt.stopPropagation();
    }
    e.elm_hover_over = over;
    e.elm_hover_out = out;
    e.addEventListener('mouseover', over);
    e.addEventListener('mouseout', out);
}

function removeHover(e) {
    if (e.elm_hover_over) {
        e.removeEventListener('mouseover', e.elm_hover_over);
    }
    if (e.elm_hover_out) {
        e.removeEventListener('mouseout', e.elm_hover_out);
    }
}

function image(props, img) {
    switch (img._0.ctor) {
    case 'Plain':   return plainImage(img._3);
    case 'Fitted':  return fittedImage(props.width, props.height, img._3);
    case 'Cropped': return croppedImage(img,props.width,props.height,img._3);
    case 'Tiled':   return tiledImage(img._3);
    }
}

function plainImage(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function tiledImage(src) {
    var div = newElement('div');
    div.style.backgroundImage = 'url(' + src + ')';
    return div;
}

function fittedImage(w, h, src) {
    var div = newElement('div');
    div.style.background = 'url(' + src + ') no-repeat center';
    div.style.webkitBackgroundSize = 'cover';
    div.style.MozBackgroundSize = 'cover';
    div.style.OBackgroundSize = 'cover';
    div.style.backgroundSize = 'cover';
    return div;
}

function croppedImage(elem, w, h, src) {
    var pos = elem._0._0;
    var e = newElement('div');
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
        var sw = w / elem._1, sh = h / elem._2;
        img.style.width = ((this.width * sw)|0) + 'px';
        img.style.height = ((this.height * sh)|0) + 'px';
        img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
        img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
    };
    img.src = src;
    img.name = src;
    e.appendChild(img);
    return e;
}

function goIn(e) { e.style.position = 'absolute'; return e; }
function goDown(e) { return e }
function goRight(e) { e.style.styleFloat = e.style.cssFloat = "left"; return e; }
function flowWith(f, array) {
    var container = newElement('div');
    if (f == goIn) container.style.pointerEvents = 'none';

    for (var i = array.length; i--; ) {
        container.appendChild(f(render(array[i])));
    }
    return container;
}

function flow(dir,elist) {
    var array = fromList(elist);
    switch(dir.ctor) {
    case "DDown":  array.reverse();
    case "DUp":    return flowWith(goDown,array);
    case "DRight": array.reverse();
    case "DLeft":  return flowWith(goRight,array);
    case "DOut":   array.reverse();
    case "DIn":    return flowWith(goIn,array);
    }
}

function toPos(pos) {
    switch(pos.ctor) {
    case "Absolute": return  pos._0 + "px";
    case "Relative": return (pos._0 * 100) + "%";
    }
}

// must clear right, left, top, bottom, and transform
// before calling this function
function setPos(pos,elem,e) {
    var element = elem.element;
    var props = elem.props;
    var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
    var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

    e.style.position = 'absolute';
    e.style.margin = 'auto';
    var transform = '';
    switch(pos.horizontal.ctor) {
    case 'P': e.style.right = toPos(pos.x); e.style.removeProperty('left'); break;
    case 'Z': transform = 'translateX(' + ((-w/2)|0) + 'px) ';
    case 'N': e.style.left = toPos(pos.x); e.style.removeProperty('right'); break;
    }
    switch(pos.vertical.ctor) {
    case 'N': e.style.bottom = toPos(pos.y); e.style.removeProperty('top'); break;
    case 'Z': transform += 'translateY(' + ((-h/2)|0) + 'px)';
    case 'P': e.style.top = toPos(pos.y); e.style.removeProperty('bottom'); break;
    }
    if (transform !== '') addTransform(e.style, transform);
    return e;
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos, elem, e);
    var div = newElement('div');
    div.style.position = 'relative';
    div.style.overflow = 'hidden';
    div.appendChild(e);
    return div;
}

function rawHtml(elem) {
    var html = elem.html;
    var args = elem.args;
    var guid = elem.guid;
    var align = elem.align;

    var div = newElement('div');
    div.innerHTML = html;
    div.style.visibility = "hidden";
    if (align) div.style.textAlign = align;
    document.body.appendChild(div);

    for (var i = args.length; i--; ) {
        var arg = args[i];
        var span = document.getElementById('md-' + guid + '-' + i);
        if (arg.isText) {
            span.innerHTML = arg;
        } else {
            span.style.display = 'block';
            span.style.width = arg.props.width + 'px';
            span.style.height = arg.props.height + 'px';
            span.appendChild(render(arg));
        }
    }
    document.body.removeChild(div);
    div.style.visibility = 'visible';
    div.style.pointerEvents = 'auto';
    return div;
}

function render(elem) { return setProps(elem, makeElement(elem)); }
function makeElement(e) {
    var elem = e.element;
    switch(elem.ctor) {
    case 'Image':     return image(e.props, elem);
    case 'Flow':      return flow(elem._0, elem._1);
    case 'Container': return container(elem._0, elem._1);
    case 'Spacer':    return newElement('div');
    case 'RawHtml':   return rawHtml(elem);
    case 'Custom':    return elem.render(elem.model);
    }
}

function update(node, curr, next) {
    if (node.tagName === 'A') { node = node.firstChild; }
    if (curr.props.id === next.props.id) return updateProps(node, curr, next);
    if (curr.element.ctor !== next.element.ctor) {
        node.parentNode.replaceChild(render(next),node);
        return true;
    }
    var nextE = next.element, currE = curr.element;
    switch(nextE.ctor) {
    case "Spacer": break;
    case "RawHtml":
        // only markdown blocks have guids, so this must be a text block
        if (nextE.guid === null) {
            if(currE.html.valueOf() !== nextE.html.valueOf()) {
                node.innerHTML = nextE.html;
            }
            break;
        }
        if (nextE.guid !== currE.guid) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nargs = nextE.args;
        var cargs = currE.args;
        for (var i = nargs.length; i--; ) {
            var narg = nargs[i];
            var carg = cargs[i]
            if (narg == carg) continue;
            var span = document.getElementById('md-' + currE.guid + '-' + i);
            if (narg.isElement) {
                if (carg.isElement) {
                    update(span, carg, narg);
                } else {
                    span.style.display = 'block';
                    var e = render(narg);
                    span.innerHTML = '';
                    span.appendChild(e);
                }
            } else {
                span.style.display = 'inline';
                span.innerHTML = narg;
            }
        }
        break;
    case "Image":
        if (nextE._0.ctor === 'Plain') {
            if (nextE._3 !== currE._3) node.src = nextE._3;
        } else if (!eq(nextE,currE) ||
                   next.props.width !== curr.props.width ||
                   next.props.height !== curr.props.height) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        break;
    case "Flow":
        var arr = fromList(nextE._1);
        for (var i = arr.length; i--; ) { arr[i] = arr[i].element.ctor; }
        if (nextE._0.ctor !== currE._0.ctor) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nexts = fromList(nextE._1);
        var kids = node.childNodes;
        if (nexts.length !== kids.length) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var currs = fromList(currE._1);
        var goDir = function(x) { return x; };
        switch(nextE._0.ctor) {
        case "DDown":  case "DUp":   goDir = goDown; break;
        case "DRight": case "DLeft": goDir = goRight; break;
        case "DOut":   case "DIn":   goDir = goIn; break;
        }
        for (var i = kids.length; i-- ;) {
            update(kids[i],currs[i],nexts[i]);
            goDir(kids[i]);
        }
        break;
    case "Container":
        update(node.firstChild, currE._1, nextE._1);
        setPos(nextE._0, nextE._1, node.firstChild);
        break;
    case "Custom":
        if (currE.type === nextE.type) {
            var done = nextE.update(node, currE.model, nextE.model);
            if (done) return;
        } else {
            return node.parentNode.replaceChild(render(next), node);
        }
    }
    updateProps(node, curr, next);
}

function updateProps(node, curr, next) {
    var props = next.props;
    var currP = curr.props;
    var e = node;
    var element = next.element;
    var width = props.width - (element.adjustWidth || 0);
    var height = props.height - (element.adjustHeight || 0);
    if (width !== currP.width) {
        e.style.width = (width|0) + 'px';
    }
    if (height !== currP.height) {
        e.style.height = (height|0) + 'px';
    }
    if (props.opacity !== currP.opacity) {
        e.style.opacity = props.opacity;
    }
    var nextColor = (props.color.ctor === 'Just' ?
                     extract(props.color._0) : '');
    if (e.style.backgroundColor !== nextColor) {
        e.style.backgroundColor = (nextColor === '' ? 'transparent' : nextColor);
    }
    if (props.tag !== currP.tag) { e.id = props.tag; }
    if (props.href !== currP.href) {
        if (currP.href === '') {
            var a = newElement('a');
            a.href = props.href;
            a.style.width = '100%';
            a.style.height = '100%';
            a.style.top = 0;
            a.style.left = 0;
            a.style.display = 'block';
            a.style.position = 'absolute';
            e.style.position = 'relative';
            a.style.pointerEvents = 'auto';
            e.appendChild(a);
        } else {
            node.lastNode.href = props.href;
        }
    }

    // update hover handlers
    if (props.hover.ctor === '_Tuple0') {
        removeHover(e);
    } else if (e.elm_hover_handler) {
        e.elm_hover_handler = props.hover;
    } else {
        addHover(e, props.hover);
    }

    // update click handlers
    if (props.click.ctor === '_Tuple0') {
        removeClick(e);
    } else if (e.elm_click_handler) {
        e.elm_click_handler = props.click;
    } else {
        addClick(e, props.click);
    }
}

return { render:render, update:update };

};
