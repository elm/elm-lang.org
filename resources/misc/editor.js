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

var elmSyntax = {
    '='   : 'defining values, pronounced &ldquo;equals&rdquo;',
    '\\'  : 'anonymous functions, pronounced &ldquo;lambda&rdquo;',
    ':'   : '<a href="/learn/Getting-started-with-Types.elm" target="_blank">' +
            'type annotations</a>, pronounced &ldquo;has type&rdquo;',
    '->'  : { message:['&ldquo;function&rdquo; in type annotations <i>or</i>',
                       'for control flow in lambdas, cases, and multi-way ifs',
                      ].join(' '),
              extra:['<p>When used in a type annotation, an arrow indicates a',
                     'function and is pronounced &ldquo;to&rdquo;. The type',
                     '<code>String -> Int</code> indicates a function from',
                     'strings to integers and is read &ldquo;String to Int&rdquo;.',
                     'You can think of a type like <code>Int -> Int -> Int</code>',
                     'as a function that takes two integer arguments and returns an integer.</p>'].join(' ')
            },
    '<-'  : 'updating fields in a record, pronounced &ldquo;gets&rdquo;',
    'as'  : 'aliasing. Can be used on imported modules and pattern complex patterns.',
    'let' : 'beginning a let expression',
    'in'  : 'marking the end of a block of definitions, and starting an expression',
    'if'  : 'beginning an conditional expression',
    'then': 'separating the conditional from the first branch',
    'else': 'separating the first and second branch',
    'case': 'beginning a case expression',
    'of'  : 'separating the expression to be pattern matched from possible case branches',
    'type': 'defining type aliases',
    'data': 'defining <a href="/learn/Pattern-Matching.elm" target="_blank">' +
            'algebraic data types (ADTs)</a>',
    '_'   : '<a href="/learn/Pattern-Matching.elm" target="_blank">pattern matching</a>' +
            ' anything, often called a &ldquo;wildcard&rdquo;',
    '..'  : 'number interpolation',
    '|'   : 'separating various things, sometimes pronounced &ldquo;where&rdquo;',
    'open': 'loading all of a modules values into local scope',
    'main': 'showing values on screen, must have type <code>Element</code> or <code>Signal Element</code>',
    'import': 'importing modules',
    'infix' : 'declaring that a given operator is non-associative',
    'infixl': 'declaring that a given operator is left-associative',
    'infixr': 'declaring that a given operator is right-associative',
    'module': 'declaring a module definition',
};

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
                         module.aliases).map(formatEntry);
    }
    return {
        values: [].concat.apply([], modules.map(parseModule)),
        modules: modules.map(function(module) {
            var desc = module.document;
            var end = desc.indexOf('\n#');
            if (end === -1) end = desc.indexOf('\n@');
            if (end !== -1) desc = desc.slice(0,end);
            module.desc = markdown.makeHtml(desc);
            return module;
        })
    };
}

function loadDoc() {
    var req = new XMLHttpRequest();
    req.onload = function () {
        elmDocs = parseDoc(JSON.parse(this.responseText));
    };
    req.open('GET', '/docs.json?v0.10', true);
    req.send();
}

function openDocPage() {
    var pos = editor.getCursor(true);
    var token = editor.getTokenAt(pos);
    if (!token.type) return;
    var result = lookupDocs(token, pos.line);
    if (!result) return;
    if (result.isSyntax) return window.open('/learn/Syntax.elm','_blank');
    if (result.isModule) return window.open(docsHref(result.home), '_blank');

    var qualifier = getQualifier(token, pos.line);
    if (qualifier) {
        result = result.filter(function(r) { return r.home == qualifier; });
    }
    if (result.length === 1) {
        return window.open(docsHref(result[0].home, result[0].name), '_blank');
    }
}

function generateView(content, cssClass) {
    var div = document.createElement("div");
    div.className = cssClass;
    div.innerHTML = content;
    return div;
}

function formatType(result) {
    var annotation = result.type;
    var firstFive = annotation.slice(0,5);
    var start = 0;
    var end = annotation.indexOf(' ')
    if (firstFive === 'type ' || firstFive === 'data ') {
        start = 5;
        end = annotation.indexOf(' ',5);
        var whitelist = ['Maybe','Either','Response','Order','Action',
                         'Time','Touch','KeyCode','FieldState'];
        if (whitelist.indexOf(result.name) === -1) {
            annotation = annotation.slice(0,end);
        }
    }
    var name = annotation.slice(start,end);
    return monospace(annotation.replace(name, docsLink(result)));
}

function docsHref(name, value) {
    var href = 'http://library.elm-lang.org/catalog/evancz-Elm/0.12/' + name.split('.').join('-');
    if (value) href = href + '#' + value;
    return href;
}

function monospace(txt) {
    return '<span style="font-family:monospace;">' + txt + '</span>';
}

function docsLink(result) {
    var value = result.name;
    var href = docsHref(result.home, value);
    var text = result.home + (value ? '.' + value : '');
    return monospace('<a href="' + href + '" target="_blank">' + text + '</a>');
}

function getQualifier (token, line) {
    var ch = token.start;
    if (ch <= 0) return null;

    var t = editor.getTokenAt({line: line, ch: ch - 1});
    if (t.type !== 'qualifier') return null;

    var previous = getQualifier(t, line);
    if (previous) return previous + '.' + t.string.slice(0,-1);
    return t.string.slice(0,-1);
}

function lookupDocs(token, line) {
    if (token.type === 'keyword' || token.string === 'main') {
        return { isSyntax:true, name:token.string };
    }
    if (token.type === 'qualifier') {
        var qualifier = getQualifier(token, line);
        var name = token.string.slice(0, -1);
        if (qualifier) {
            name = qualifier + '.' + name;
        }
        var matches = elmDocs.modules.filter(function(m) { return m.name == name; });
        return matches.length === 0 ? null : { isModule:true, home:name, desc:matches[0].desc };
    }
    if (token.string) {
        var results = elmDocs.values.filter(function(x) { return x.name == token.string; });
        if (results.length > 0) return results;
        results = elmDocs.modules.filter(function(m) { return m.name === token.string; });
        if (results.length === 1) return { isModule:true, home:results[0].name, desc:results[0].desc };
    }
    return null;
}

function messageForTokenAt(pos) {
    var token = editor.getTokenAt(pos);

    var empty = { message:'', extra:'' };
    if (!token.type) return empty;

    var results = lookupDocs(token, pos.line);
    if (results === null) return empty;
    if (results.isSyntax) {
        var info = elmSyntax[results.name];
        var desc = info.message || info;
        var extra = '<p>See the <a href="/learn/Syntax.elm">syntax reference</a> for more information.</p>';
        return {
            message: '<a href="/learn/Syntax.elm">Built-in syntax</a>' + (desc ? ' for ' + desc : ''),
            extra: (info.extra ? info.extra : '') + extra
        };
    }
    if (results.isModule) {
        return {
            message: 'Module ' + docsLink(results),
            extra: results.desc
        };
    }
    var qualifier = getQualifier(token, pos.line);
    if (qualifier) {
        results = results.filter(function(result) { return result.home == qualifier; });
    }

    if (results.length === 0) return empty;
    if (results.length === 1) {
        var value = results[0];
        return { message:formatType(value),
                 extra: value.desc ? value.desc : '<p>No additional information.</p>' };
    }

    return {
        message: 'You probably want one of these: ' +
            results.map(docsLink).join(' or '),
        extra: '<p>This feature is not yet clever enough to figure out which one.</p>'
    };
}

function updateDocumentation() {
    var message = messageForTokenAt(editor.getCursor(true));
    var boxes = document.getElementById('documentation').childNodes;
    boxes[0].childNodes[0].innerHTML = message.message;
    boxes[0].childNodes[1].style.display = message.message ? 'block' : 'none';
    boxes[1].innerHTML = message.extra;
    boxes[1].style.display = mode.verbose && message.extra ? 'block' : 'none';
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
    document.getElementById('show_type_checkbox').checked = show;
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
    if (!mode.verbose) showType(true);
    document.getElementById('toggle_link').innerHTML = mode.verbose ? 'more' : 'less';
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
    document.getElementById('toggle_link').innerHTML = 'more';
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
    for (var i = classes.length; i--; ) {
        if (!(classes[i].match(/^zoom-/))) newClasses.push(classes[i]);
    }
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
    editor = CodeMirror.fromTextArea(
        document.getElementById('input'),
        { lineNumbers: initLines(),
          matchBrackets: true,
          theme: initTheme(),
          tabMode: 'shift',
          extraKeys: {
              'Ctrl-Enter': compile,
              'Shift-Ctrl-Enter': hotSwap,
              'Ctrl-H': toggleVerbose,
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
