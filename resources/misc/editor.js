var editor = null;
var elmDocs = null;

function compile() {
    var form = document.getElementById('inputForm');
    form.submit();
}

function hotSwap() {
    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
        if (request.readyState === 4
            && request.status >= 200
            && request.status < 300) {
            var result = JSON.parse(request.responseText);
            var top = self.parent;
            if (js = result.success) {
                var error = top.output.document.getElementById('ErrorMessage');
                if (error) {
                    error.parentNode.removeChild(error);
                }
                top.output.eval(js);
                var module = js.substring(0,js.indexOf('=')).replace(/\s/g,'');
                top.output.runningElmModule =
                    top.output.runningElmModule.swap(top.output.eval(module));
            } else {
                var error = top.output.document.getElementById('ErrorMessage');
                if (!error) {
                    error = document.createElement('div');
                    error.id = 'ErrorMessage';
                    error.style.fontFamily = 'monospace';
                    error.style.position = 'absolute';
                    error.style.bottom = '0';
                    error.style.width = '100%';
                    error.style.backgroundColor = 'rgba(245,245,245,0.95)';
                }
                error.innerHTML = '<b>Hot Swap Failed</b><br/>' +
                    result.error.replace(/\n/g, '<br/>').replace(/  /g, " &nbsp;");
                top.output.document.body.appendChild(error);
            }
        }
    };
    editor.save();
    var elmSrc = encodeURIComponent(document.getElementById('input').value);
    request.open('POST', '/hotswap?input=' + elmSrc, true);
    request.setRequestHeader('Content-Type', 'application/javascript');
    request.send();
}

function adjustEditorBottom() {
  var options = document.getElementById('options');
  var edb = document.getElementById('editor_box');
  edb.style.bottom = options.clientHeight + 'px';
  editor.refresh();
}

function parseDoc(modules) {
    var markdown = new Showdown.converter();
    function parseModule(module) {
        function formatEntry(entry) {
            return { name: entry.name,
                     type: entry.raw,
                     home: module.name,
                     desc: markdown.makeHtml(entry.comment)
            };
        }
        return [].concat(module.values,
                         module.datatypes,
                         module.datatypes).map(formatEntry);
    }
    return {
        values: [].concat.apply([], modules.map(parseModule)),
        modules: modules
    };
}

function loadDoc() {
  var req = new XMLHttpRequest();
  req.onload = function () {
      console.log(this.responseText);
      elmDocs = parseDoc(JSON.parse(this.responseText));
  };
  req.open('GET', '/docs.json?v0.10', true);
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
    console.log('this one?', module, anchor, text, hoverText);
    return '<a href="' + moduleRef(module) + anchorLink + '" target="elm-docs" title="' + titleText + '">' + linkText + '</a>';
}

function moduleRef (module) {
  var parts = module.split('.');
  var ref = null;
  if (module === 'Syntax') {
    ref = '/learn/Syntax.elm';
  } else {
    ref = 'http://docs.elm-lang.org/library/' + parts.join('/') + '.elm';
  }
  return ref;
}

function lookupDocs(token, line) {
    var matches;
    if (token.type == 'keyword') {
        matches = [{
            name: token.string,
            type: 'Keyword',
            module: 'Syntax'
        }];
    } else if (token.type == 'qualifier') {
        var qualifier = getQualifier(token, line);
        matches = [{
            module: (qualifier ? qualifier + '.' : '') + token.string.slice(0, -1),
            type: 'Module'
        }];
    } else {
        if (token.string) {
            matches = elmDocs.values.filter(function(x) { return x.name == token.string; });
        }
    }
    return matches;
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

function generateView(content, cssClass) {
    var div = document.createElement("div");
    div.className = cssClass;
    div.innerHTML = content;
    return div;
}

function getDocForTokenAt (pos) {
    var doc = null;
    var token = editor.getTokenAt(pos);
    if (!token.type) return null;
    var results = lookupDocs(token, pos.line);

    if (results.length === 0) return null;
    if (results.length === 1) return results[0];

    var qualifier = getQualifier(token, pos.line);
    if (qualifier) {
        return results.filter(function(result) { return result.home == qualifier; })[0];
    }

    function toLink(result) {
        return moduleToHtmlLink(result.module, token.string);
    }
    return { error: 'Ambiguous: ' + token.string + ' defined in ' + results.map(toLink).join(' and ') };
}

function formatType(result) {
    return result.type;
}

function updateDocumentation() {
  var result = getDocForTokenAt(editor.getCursor(true));
  var type = '';
  var desc = '';

  if (result !== null) {
      type = result.error ? result.error : formatType(result);
      desc = result.comment ? result.comment : '<p>No description found</p>';
  }
  var docs = document.getElementById('documentation').childNodes;
  docs[0].innerHTML = type;
  docs[1].innerHTML = desc;
  docs[1].style.display = mode.verbose && desc ? 'block' : 'none';
  adjustView(mode);
}

var Mode = { OPTIONS:0, TYPES:1, NONE:2 };
var mode = { mode: Mode.NONE };

function showOptions(show) {
    if (mode.mode === Mode.OPTIONS) {
        mode = show ? mode : mode.hidden;
    } else {
        mode = show ? { mode: Mode.OPTIONS, hidden: mode } : mode;
    }
    adjustView(mode);
}

function showType(show) {
    cookie('showtype', show);
    var newMode = (show ? { mode: Mode.TYPES, verbose: false }
                        : { mode: Mode.NONE });
    if (mode.mode === Mode.OPTIONS) {
        mode.hidden = newMode;
    } else {
        mode = newMode;
    }
    adjustView(mode);
}

function toggleVerbose() {
    mode.verbose = !mode.verbose;
    updateDocumentation();
}

function showVerbose() {
    mode.verbose = true;
    updateDocumentation();
}

function hideStuff() {
    if (mode.hidden) mode = mode.hidden;
    document.getElementById('options_checkbox').checked = false;
    mode.verbose = false;
    updateDocumentation();
}

function adjustView(mode) {
    switch (mode.mode) {
    case Mode.OPTIONS:
        setVisibility('editor_options', true);
        setVisibility('documentation', false);
        adjustEditorBottom();
        return;
    case Mode.TYPES:
        setVisibility('editor_options', false);
        setVisibility('documentation', true);
        adjustEditorBottom();
        return;
    case Mode.NONE:
        setVisibility('editor_options', false);
        setVisibility('documentation', false);
        adjustEditorBottom();
        return;
    }
}

function setVisibility(id, visible) {
  var node = document.getElementById(id);
  node.style.display = visible ? 'block' : 'none';
  node.style.borderTopWidth = visible ? '1px' : '0';
}

function showLines(on) {
  editor.setOption('lineNumbers', on);
  cookie('lineNumbers', on);
}

var delay;
function setAutoHotSwap(enable, init) {
    document.getElementById('hot_swap_button').disabled = enable;
    if (enable) {
        if (!init) updateOutput();
        editor.on('change', updateOutput);
    } else {
        editor.off('change', updateOutput);
    }
    cookie('autoHotSwap', enable);
}

function updateOutput() {
    clearTimeout(delay);
    delay = setTimeout(hotSwap, 1000);
}

function setTheme(theme) {
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
  editor.refresh();
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
    lines = lines === 'true';
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

function initMenu() {
  loadDoc();
  var stored = readCookie('showtype');
  var show = !stored || stored === 'true';
  document.getElementById('show_type_checkbox').checked = show;
  showType(show);
}

function isBounceInIFrame() {
    return window.location.pathname.indexOf("Bounce") >= 0 && window !== window.top;
}

function initAutoHotSwap() {
    var status = readCookie('autoHotSwap') === 'true' || isBounceInIFrame();
    document.getElementById('auto_hot_swap_checkbox').checked = status;
    setAutoHotSwap(status, true);
}

function initEditor() {
  // global scope editor
  editor = CodeMirror.fromTextArea(document.getElementById('input'),
    { lineNumbers: initLines(),
      matchBrackets: true,
      theme: initTheme(),
      tabMode: 'shift',
      extraKeys: {'Ctrl-Enter': compile,
                  'Shift-Ctrl-Enter': hotSwap,
                  'Ctrl-K': toggleVerbose,
                  'Shift-Ctrl-K': openDocPage,
                  'Tab': function(cm) {
                          var spaces = Array(cm.getOption("indentUnit") + 1).join(" ");
                          cm.replaceSelection(spaces, "end", "+input");
                      }
       }
    });
  if (!isBounceInIFrame()) editor.focus();
  editor.on('cursorActivity', hideStuff);
  initZoom();
  initMenu();
  initAutoHotSwap();
}

/* jshint browser: true */
/* jshint devel: true */
/* jshint undef: true */
/* jshint unused: true */
/* jshint unused: true */
/* global CodeMirror */
/* global Showdown */
/* exported initEditor */
/* exported showOptions */
/* exported showLines */
/* exported setTheme */
/* exported eraseCookie */
