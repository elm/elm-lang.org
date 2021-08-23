(function() {

  // POLYFILL

  window.requestIdleCallback =
    window.requestIdleCallback ||
    function (cb) {
      var start = Date.now();
      return setTimeout(function () {
        cb({
          didTimeout: false,
          timeRemaining: function () {
            return Math.max(0, 50 - (Date.now() - start));
          }
        });
      }, 1);
    }

  window.cancelIdleCallback =
    window.cancelIdleCallback ||
    function (id) {
      clearTimeout(id);
    }


  // DEBOUNCER

  var debounce = function(func) {
    var token;
    return function() {
      var later = function() {
        token = null;
        func.apply(null, arguments);
      };
      cancelIdleCallback(token);
      token = requestIdleCallback(later);
    };
  };


  // EDITOR

  function CodeEditor() {
    this._editor = null;
    this._theme = 'light';
    this._source = null;
    this._start = null;
    this._end = null;
    this._importEnd = 0;

    this._init = this._init.bind(this);
    this._updateTheme = this._updateTheme.bind(this);
    this._updateSource = this._updateSource.bind(this);
    this._updateCursor = this._updateCursor.bind(this);

    return Reflect.construct(HTMLElement, [], this.constructor);
  }

  CodeEditor.prototype = Object.create(HTMLElement.prototype, {
    constructor: {
      value: CodeEditor
    },

    connectedCallback: {
      value: function () {
        this._init();
      }
    },

    disconnectedCallback: {
      value: function () {
        this._editor = null;
        this._theme = 'light';
        this._source = null;
        this._start = null;
        this._end = null;
        this._importEnd = 0;
      }
    },


    // INIT EDITOR

    _init: {
      value: function () {

        var sendChangeEvent = debounce((function() {
          var previous = this._source;
          this._source = this._editor.getValue();
          if (previous === this._source) return;
          this.dispatchEvent(new Event('change'));
        }).bind(this));

        var sendSaveEvent = debounce((function() {
          this.dispatchEvent(new Event('save'));
        }).bind(this));

        var sendHintEvent = (function() {
          this.dispatchEvent(new Event('hint'));
        }).bind(this);

        this._editor = CodeMirror(this, {
          lineNumbers: true,
          matchBrackets: true,
          styleActiveLine: true,
          theme: this._theme,
          value: this._source,
          tabSize: 2,
          indentWithTabs: false,
          extraKeys: {
            "'\''": handleOpen("'", "'"),
            "'\"'": handleOpen('"', '"'),
            "'('": handleOpen('(', ')'),
            "'{'": handleOpen('{', '}'),
            "'['": handleOpen('[', ']'),
            "')'": handleClose(')'),
            "'}'": handleClose('}'),
            "']'": handleClose(']'),
            "Tab": handleTab,
            "Shift-Tab": handleUntab,
            "Backspace": handleBackspace,
            'Cmd-S': function(cm) { sendSaveEvent(); },
            "Ctrl-Enter": function(cm) { sendSaveEvent(); }
          }
        });

        this._editor.on('changes', sendChangeEvent);
        this._editor.focus();
        requestIdleCallback((function() {
          // Make sure Elm is ready to receive messages.
          this._editor.on('cursorActivity', sendHintEvent)
        }).bind(this));

        this._updateSource();
        this._updateCursor();
      }
    },


    // UPDATE EDITOR

    _updateSource: {
      value: function () {
        if (!this._editor) return;

        this._editor.setValue(this._source);
      }
    },

    _updateTheme: {
      value: function () {
        if (!this._editor) return;

        this._editor.setOption('theme', this._theme);
      }
    },

    _updateCursor: {
      value: function () {
        if (!this._editor) return;

        var isStartNull = isPositionNull(this._start);
        var isEndNull = isPositionNull(this._end);

        if (!(isStartNull && isEndNull)) {
          var start_ = isStartNull ? this._end : this._start;
          var end_ = isEndNull ? this._start : this._end;
          var start = { line: start_.line - 1, ch: start_.column - 1 };
          var end = { line: end_.line - 1, ch: end_.column - 1 }
          this._editor.setSelection(start, end, { scroll: false });
          this._editor.scrollIntoView({ from: start, to: end }, 200);
          this._editor.focus();
          this._start = null;
          this._end = null;
        }
      }
    },


    // PROPERTY: SOURCE

    source: {
      get: function () {
        return this._source;
      },
      set: function (updated) {
        var oldSource = this._source;
        this._source = updated;

        if (updated !== oldSource) {
          this._updateSource();
        }
      }
    },


    // PROPERTY: THEME

    theme: {
      get: function () {
        return this._theme;
      },
      set: function (updated) {
        var oldTheme = this._theme;
        this._theme = updated;

        if (updated !== oldTheme) {
          this._updateTheme();
        }
      }
    },


    // PROPERTY: CURSOR

    selection: {
      get: function () {
        return { start: this._start, end: this._end };
      },
      set: function (updated) {
        var oldStart = this._start;
        var oldEnd = this._end;
        this._start = updated.start;
        this._end = updated.end;

        var isSame = isPositionEqual(updated.start, oldStart) && isPositionEqual(updated.end, oldEnd);
        if (!isSame) { this._updateCursor(); }
      }
    },


    // PROPERTY: IMPORT END

    importEnd: {
      get: function () {
        return this._importEnd;
      },
      set: function (updated) {
        this._importEnd = updated;
      }
    },


    // PROPERTY: HINT

    hint: {
      get: function () {
        if (!this._editor) return null;

        return getHint(this._editor, this._importEnd);
      }
    }

  });

  // HELPERS

  function isPositionNull(position) {
    return position === null || position.line === null || position.column === null;
  }

  function isPositionEqual(a, b) {
    return (b && b.line) === (a && a.line) && (b && b.column) === (a && a.column);
  }


  // CODEMIRROR: EXTRA KEYS

  function handleTab(cm) {
    cm.execCommand("indentMore");
  }

  function handleUntab(cm) {
    cm.execCommand("indentLess");
  }

  function handleBackspace(cm) {
    var ranges = cm.listSelections();
    for (var i = 0; i < ranges.length; i++) {
      var range = ranges[i];
      if (!range.empty()) {
        return CodeMirror.Pass;
      }
      var pos = range.head;
      var pair = cm.getRange(
        CodeMirror.Pos(pos.line, pos.ch - 1),
        CodeMirror.Pos(pos.line, pos.ch + 1)
      );
      if (pair != '()' && pair != '{}' && pair != '[]' && pair != '""' && pair != "''") {
        return CodeMirror.Pass;
      }
    }
    for (var i = ranges.length; i--; ) {
      var pos = ranges[i].head;
      cm.replaceRange("",
        CodeMirror.Pos(pos.line, pos.ch - 1),
        CodeMirror.Pos(pos.line, pos.ch + 1),
        "+delete"
      );
    }
  }

  function handleOpen(open, close) {
    return function(cm) {
      var pairCount = 0;
      var surroundCount = 0;
      var ranges = cm.listSelections();
      for (var i = 0; i < ranges.length; i++) {
        var range = ranges[i];
        if (!range.empty()) {
          surroundCount++;
          continue;
        }

        var pos = range.head;
        if (close == '"') {
          var token = cm.getTokenAt(pos);
            if (token.type == 'string' || token.type == 'error') {
              return handleClose(close)(cm);
            }
        }
        var next = cm.getRange(pos, CodeMirror.Pos(pos.line, pos.ch + 1));
        if (next === '' || /[ \v\f\(\)\{\}\[\]]/.test(next)) {
          pairCount++;
        }
      }

      if (pairCount === ranges.length) {
        cm.operation(function() {
          cm.replaceSelection(open + close, null);
          cm.triggerElectric(open + close);
          cm.execCommand("goCharLeft");
        });
      } else if (surroundCount === ranges.length) {
        cm.operation(function() {
          var selections = cm.getSelections();
          for (var i = 0; i < selections.length; i++) {
            selections[i] = open + selections[i] + close;
          }
          cm.replaceSelections(selections, "around");
          selections = cm.listSelections().slice();
          for (var i = 0; i < selections.length; i++) {
            var anchor = selections[i].anchor;
            var head = selections[i].head;
            var one = anchor.line < head.line
              ? 1
              : anchor.line === head.line && anchor.ch <= head.ch ? 1 : -1;
            selections[i] = {
              anchor: new CodeMirror.Pos(anchor.line, anchor.ch - one),
              head: new CodeMirror.Pos(head.line, head.ch + one)
            };
          }
          cm.setSelections(selections);
        });
      } else {
        return open == close ? handleClose(close)(cm) : CodeMirror.Pass;
      }
    };
  }

  function handleClose(close) {
    return function(cm) {
      var ranges = cm.listSelections();
      for (var i = 0; i < ranges.length; i++) {
        var range = ranges[i];
        var pos = range.head;
        var next = cm.getRange(pos, CodeMirror.Pos(pos.line, pos.ch + 1));
        if (range.empty() && next == close) {
          continue;
        } else {
          return CodeMirror.Pass;
        }
      }
      cm.execCommand("goCharRight");
    }
  }


  // GET HINT

  function getHint(editor, importEnd) {
    var start = editor.getCursor('anchor');
    var cursor = editor.getCursor('head');

    if (start.line !== cursor.line || start.ch !== cursor.ch) {
      return null;
    }

    var line = cursor.line;
    var token = editor.getTokenAt(cursor);
    var type = token.type;
    return type === 'variable' ? getLowerHint(editor, line, token)
      : type === 'variable-2' ? getUpperHint(editor, importEnd, line, token)
      : type === 'keyword' ? getKeywordHint(editor, importEnd, line, token)
      : type === 'def' ? getDefHint(token.string)
      : null;
  }

  function getLowerHint(editor, line, token) {
    return getPrefix(editor, line, token) + token.string;
  }

  function getUpperHint(editor, importEnd, line, token) {
    var name = getPrefix(editor, line, token) + token.string + getPostfix(editor, line, token);
    if (line < importEnd) {
      var content = editor.getLine(line);
      if (!/^import\b/.test(content)) return name;
      var i1 = content.indexOf('as');
      var i2 = content.indexOf('exposing');
      if (i1 < 0 && i2 < 0) return 'module:' + name;
      if (i1 > 0 && token.end < i1) return 'module:' + name;
      if (i2 > 0 && token.end < i2) return 'module:' + name;
    }
    return name;
  }

  function getKeywordHint(editor, importEnd, line, token) {
    switch (token.string) {
      case '.':
        var next = getTokenAfter(editor, line, token);
        return next.type === 'variable' ? getLowerHint(editor, line, next)
          : next.type === 'variable-2' ? getUpperHint(editor, line, next) : null;

      case 'type':
        return /^type\salias\b/.test(editor.getLine(line)) ? 'alias' : 'type';

      case 'as':
        return line < importEnd ? 'import' : 'as';

      default:
        return token.string;
    }
  }

  function getDefHint(name) {
    return name === 'init' || name === 'view' || name === 'update' || name === 'subscriptions'
      ? 'def:' + name
      : null;
  }

  function getPrefix(editor, line, token) {
    var dot = getTokenBefore(editor, line, token);
    if (dot.string !== '.') { return ''; }

    var qualifier = getTokenBefore(editor, line, dot);
    return qualifier.type === 'variable-2'
      ? getPrefix(editor, line, qualifier) + qualifier.string + '.'
      : '.';
  }

  function getPostfix(editor, line, token) {
    var dot = getTokenAfter(editor, line, token);
    if (dot.string !== '.') { return ''; }

    var next = getTokenAfter(editor, line, dot);
    return next.type === 'variable'
      ? '.' + next.string
      : next.type === 'variable-2'
        ? '.' + next.string + getPostfix(editor, line, next)
        : '.';
  }

  function getTokenBefore(editor, line, token) {
    return editor.getTokenAt({ line: line, ch: token.start });
  }

  function getTokenAfter(editor, line, token) {
    return editor.getTokenAt({ line: line, ch: token.end + 1 });
  }

  window.customElements.define('code-editor', CodeEditor);

})();