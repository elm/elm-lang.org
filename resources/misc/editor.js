var editor = null;
var elmDocs = null;

function compile(formTarget) {
  var form = document.getElementById('inputForm');
  form.target = formTarget;
  form.submit();
}

function setEditorBottom() {
  var typeView = document.getElementById('doc_type');
  var opts = document.getElementById('editor_options');
  var edb = document.getElementById('editor_box');
  visible = typeView.style.visibility === 'visible' || opts.style.visibility === 'visible';
  edb.style.bottom = visible ? '62px' : '36px';
  editor.refresh();
}

function showTypeView() {
  var typeView = document.getElementById('doc_type');
  typeView.style.visibility = 'visible';
  setEditorBottom();
}

function hideTypeView() {
  var typeView = document.getElementById('doc_type');
  typeView.style.visibility = 'hidden';
  setEditorBottom();
}

function formatType(tipe) {
    return tipe.replace(/\(Number a\)/g, 'number')
               .replace(/Number a/g, 'number')
               .replace(/\(Appendable a\)/g, 'appendable')
               .replace(/Appendable a/g, 'appendable')
               .replace(/\(Comparable [ak]\)/g, 'comparable')
               .replace(/Comparable [ak]/g, 'comparable');
}

function parseDoc(mods) {
  var markdown = new Showdown.converter();
  var ds = mods.modules.map(function (m) {
    var fs = m.values.map(function (f) {
      return {name: f.name,
              type: formatType(f.type),
              module: m.name,
              desc: markdown.makeHtml(f.desc)};
    });
    return fs;
  });

  var result = {};
  result.docs = ds.reduce(function (acc, val) { return acc.concat(val); }, []);
  result.modules = mods.modules;
  return result;
}

function loadDoc () {
  var req = new XMLHttpRequest();
  req.onload = function () { elmDocs = parseDoc(JSON.parse(this.responseText)); };
  req.open('GET', '/jsondocs', true);
  req.send();
}

function wrapIfOperator(name) {
  var nonSymbols = /[A-Za-z0-9]/;
  if (name.match(nonSymbols)) {
    return name;
  } else {
    //  Best bet is an operator that needs () wrapping
    return '(' + name + ')';
  }
}

function moduleToHtmlLink(module, anchor, text, hoverText) {
  var linkText = text || module;
  var titleText = hoverText || '';
  var anchorLink = anchor ? '#' + wrapIfOperator(anchor) : '';
  return '<a href="' + moduleRef(module) + anchorLink + '" target="elm-docs" title="' + titleText + '">' + linkText + '</a>';
}

function moduleRef (module) {
  var parts = module.split('.');
  var ref = null;
  if (module === 'Syntax') {
    ref = '/learn/Syntax.elm';
  } else {
    ref = '/docs/' + parts.join('/') + '.elm';
  }
  return ref;
}

function lookupDocs(token, line) {
  var ds = null;
  if (token.type == 'keyword') {
    ds = [{
      name: token.string,
      type: 'Keyword',
      module: 'Syntax'
    }];
  } else if (token.type == 'qualifier') {
    var q = getQualifier(token, line);
    var module = q ? q + '.' + token.string.slice(0, -1) : token.string.slice(0, -1);
    ds = [{
      module: module,
      type: 'Module'
    }];
  } else {
    if (token.string) {
      ds = elmDocs.docs.filter(function(x) { if (x.name == token.string) return true; });
    }
  }
  return ds;
}

function getQualifier (token, line) {
  var ch = token.start;
  if (ch > 0) {
    var t = editor.getTokenAt({line: line, ch: ch - 1});
    if (t.type == 'qualifier') {
      var previous = getQualifier(t, line);
      if (previous) {
        return previous + '.' + t.string.slice(0, -1);
      } else {
        return t.string.slice(0, -1);
      }
    }
  }
  return null;
}

function openDocPage () {
  var current_pos = editor.getCursor(true);
  var token = editor.getTokenAt(current_pos);
  var ds = token.type ? lookupDocs(token, current_pos.line) : null;
  var ref = null;
  if (ds && ds.length > 0) {
    if (ds.length > 1) {
      var q = getQualifier(token, current_pos.line);
      if (q) {
        ref = moduleRef(ds.filter(function(o) { if (o.module == q) return true;})[0].module + '#' + wrapIfOperator(ds[0].name));
      }
    } else {
      ref = moduleRef(ds[0].module) + '#' + wrapIfOperator(ds[0].name);
    }
  }
  if (ref) {
    window.open(ref, 'elm-docs');
  }
}

function clearView(id) {
  var elem = document.getElementById(id);
  if (elem) {
    elem.innerHTML = '';
  }
}

function clearDocView () {
  clearView('doc_desc');
}

function generateView (content, contentIsHtml, cssClass) {
    var div = document.createElement("div");
    div.className = cssClass;
    if (contentIsHtml) {
      div.innerHTML = content;
    } else {
      div.appendChild(document.createTextNode(content));
    }
    return div;
}

function getDocForTokenAt (pos) {
  var doc = null;
  var token = editor.getTokenAt(pos);
  var docs = token.type ? lookupDocs(token, pos.line) : null;

  if (docs && docs.length > 0) {
    if (docs.length > 1) {
      var q = getQualifier(token, pos.line);
      if (q) {
        doc = docs.filter(function(o) { if (o.module == q) return true;})[0];
      } else {
        doc = {};
        doc.error = 'Ambiguous: ' + token.string + ' defined in ' + docs.map(function(o) { return moduleToHtmlLink(o.module, token.string); }).join(' and ');
      }
    } else {
      doc = docs[0];
    }
  }
  return doc;
}

function typeAsText (doc) {
  var result =  '';
  result += doc.module ? doc.module : '';
  result += (doc.module && doc.name) ? '.' : '';
  result += doc.name ? doc.name : '';
  result = moduleToHtmlLink(doc.module, doc.name, result);
  result += doc.type ? ' : ' + doc.type : '';
  return result;
}

function showDoc () {
  var current_pos = editor.getCursor(true);
  clearView('doc_desc');

  var doc = getDocForTokenAt(current_pos);
  var typeText = "";
  var desc = "";

  if (doc && doc.error) {
    typeText = doc.error;
  } else if (doc) {
    typeText = typeAsText(doc);
    desc = doc.desc;
  } else {
    return;
  }

  if (!desc || desc === "") {
    desc = 'No description found';
  }

  var type_div = generateView(typeText, true, 'doc_type');
  var doc_div = generateView(desc, true, 'doc');
  var docView = document.getElementById('doc_desc');

  var vscroll = document.getElementsByClassName('CodeMirror-vscrollbar')[0];
  if (vscroll && vscroll.offsetWidth  > 0) {
    docView.style.marginRight = vscroll.offsetWidth + "px";
  }

  docView.appendChild(type_div);
  docView.appendChild(doc_div);
  docView.style.visibility = 'visible';
}

function hideDocView() {
  var docView = document.getElementById('doc_desc');
  docView.style.visibility = 'hidden';
}

function toggleDocView () {
  var docView = document.getElementById('doc_desc');
  if (docView.style.visibility == 'visible') {
    hideDocView();
  } else {
    showDoc();
  }
}

function updateTypeView () {
  var current_pos = editor.getCursor(true);

  clearView('doc_type');

  var doc = getDocForTokenAt(current_pos);
  var typeText = "";

  if (doc && doc.error) {
    typeText = doc.error;
  } else if (doc) {
    typeText = typeAsText(doc);
  }

  var type_div = generateView(typeText, true, 'doc_type');
  var typeView = document.getElementById('doc_type');
  typeView.appendChild(type_div);
}

function toggleShowType(enable) {
  if (enable) {
    showTypeView();
    editor.on('cursorActivity', updateTypeView);
    updateTypeView();
  } else {
    hideTypeView();
    editor.off('cursorActivity', updateTypeView);
  }
  cookie('showtype', enable);
}

function toggleOptions(show) {
  var opts = document.getElementById('editor_options');
  opts.style.visibility = show ? 'visible' : 'hidden';
  setEditorBottom();
}

function toggleLines(on) {
  editor.setOption('lineNumbers', on);
  cookie('lineNumbers', on);
}

var delay;
function toggleAutoCompile(enable) {
  document.getElementById('compile_button').disabled = enable;
  if (enable) {
    updateOutput();
    editor.on('change', updateOutput);
  } else {
    editor.off('change', updateOutput);
  }
  cookie('autocompile', enable);
}

function updateOutput() {
  clearTimeout(delay);
  delay = setTimeout(compileOutput, 1000);
}

function compileOutput() {
  compile('output');
}

function setTheme() {
  var input = document.getElementById('editor_theme');
  var theme = input.options[input.selectedIndex].innerHTML;
  editor.setOption('theme', theme);
  cookie('theme', theme);
}

function setZoom() {
  var editorDiv  = document.getElementsByClassName('CodeMirror')[0],
      classes    = editorDiv.getAttribute('class').split(' '),
      input      = document.getElementById('editor_zoom'),
      zoomLevel  = input.options[input.selectedIndex].innerHTML,
      zoom       = 'zoom-' + zoomLevel.slice(0,-1),
      newClasses = [];
  for (var i = classes.length; i--; )
    if (!(classes[i].match(/^zoom-/)))
      newClasses.push(classes[i]);
  newClasses.push(zoom);
  editorDiv.setAttribute('class', newClasses.join(' '));
  cookie('zoom', zoomLevel);
}

function cookie(name,value) { createCookie(name, value, 5*365); }

function createCookie(name,value,days) {
  var expires = "";
  if (days) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    expires = "; expires=" + date.toGMTString();
  }
  document.cookie = name + "=" + value + expires + "; path=/";
}

function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) === 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
}

function eraseCookie(name) {
  createCookie(name,"",-1);
}

function initLines() {
  var lines = readCookie('lineNumbers');
  if (lines) {
    lines = lines == 'true';
    document.getElementById('editor_lines').checked = lines;
    return lines;
  }
  return false;
}

function initTheme() {
  var theme = readCookie('theme');
  if (theme) {
    document.getElementById('editor_theme').value = theme;
    return theme;
  }
  return 'vibrant-ink';
}

function initZoom() {
  var zoom = readCookie('zoom');
  if (zoom) {
    document.getElementById('editor_zoom').value = zoom;
    setZoom();
  }
}

function initTypeView() {
  var stored = readCookie('showtype');
  var showType = stored ? stored === 'true' : true;
  if (showType) {
    document.getElementById('show_type_checkbox').checked = showType;
    toggleShowType(showType);
  }
  var doc_type = document.getElementById('doc_type');
  doc_type.onclick = openDocPage;
  loadDoc();
}

function initAutocompile() {
  var auto = readCookie('autocompile') == 'true';
  if (auto) {
    document.getElementById('autocompile_checkbox').checked = auto;
    toggleAutoCompile(auto);
  }
}

function initOptions() {
  var options = document.getElementById('options_checkbox');
  options.checked = false;
  toggleOptions(document.getElementById('options_checkbox').checked);
}

function initEditor() {
  // global scope editor
  editor = CodeMirror.fromTextArea(document.getElementById('input'),
    { lineNumbers: initLines(),
      matchBrackets: true,
      theme: initTheme(),
      tabMode: 'shift',
      extraKeys: {'Ctrl-Enter': compileOutput, 'Ctrl-K': toggleDocView, 'Shift-Ctrl-K': openDocPage }
    });
  editor.focus();
  editor.on('cursorActivity', hideDocView);
  initAutocompile();
  initTypeView();
  initZoom();
  initOptions();
}

/* jshint browser: true */
/* jshint devel: true */
/* jshint undef: true */
/* jshint unused: true */
/* jshint unused: true */
/* global CodeMirror */
/* global Showdown */
/* exported initEditor */
/* exported toggleOptions */
/* exported toggleLines */
/* exported setTheme */
/* exported eraseCookie */
