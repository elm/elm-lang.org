
// NODES


var codeNode = document.getElementById('code');
var editorNode = document.getElementById('editor');
var optionsNode = document.getElementById('options');
var dividerNode = document.getElementById('divider');
var outputNode = document.getElementById('output');



// BUTTONS


function lights()
{
	if (editor.getOption('theme') === 'dark')
	{
		optionsNode.style.background = '#ACAC96';
		editor.setOption('theme', 'light');
	}
	else
	{
		optionsNode.style.background = '#0B0E01';
		editor.setOption('theme', 'dark');
	}
}

function compile()
{
	var source = editor.getValue();
	codeNode.value = source;
	editorNode.submit();
	hints && hints.ports.submissions.send(source);
}



// EDITOR


var editor = CodeMirror.fromTextArea(codeNode, {
	lineNumbers: true,
	matchBrackets: true,
	theme: 'dark',
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
		"Ctrl-Enter": compile
	}
});
editor.focus();



// EXTRA KEYS


function handleTab(cm)
{
	cm.execCommand("indentMore");
}

function handleUntab(cm)
{
	cm.execCommand("indentLess");
}

function handleBackspace(cm)
{
	var ranges = cm.listSelections();
	for (var i = 0; i < ranges.length; i++)
	{
		var range = ranges[i];
		if (!range.empty())
		{
			return CodeMirror.Pass;
		}
		var pos = range.head;
		var pair = cm.getRange(
			CodeMirror.Pos(pos.line, pos.ch - 1),
			CodeMirror.Pos(pos.line, pos.ch + 1)
		);
		if (pair != '()' && pair != '{}' && pair != '[]' && pair != '""' && pair != "''")
		{
			return CodeMirror.Pass;
		}
	}
	for (var i = ranges.length; i--; )
	{
		var pos = ranges[i].head;
		cm.replaceRange("",
			CodeMirror.Pos(pos.line, pos.ch - 1),
			CodeMirror.Pos(pos.line, pos.ch + 1),
			"+delete"
		);
	}
}

function handleOpen(open, close)
{
	return function(cm)
	{
		var pairCount = 0;
		var surroundCount = 0;
		var ranges = cm.listSelections();
		for (var i = 0; i < ranges.length; i++)
		{
			var range = ranges[i];
			if (!range.empty())
			{
				surroundCount++;
				continue;
			}

			var pos = range.head;
			if (close == '"')
			{
				var token = cm.getTokenAt(pos);
			    if (token.type == 'string' || token.type == 'error')
			    {
			    	return handleClose(close)(cm);
			    }
			}
			var next = cm.getRange(pos, CodeMirror.Pos(pos.line, pos.ch + 1));
			if (next === '' || /[ \v\f\(\)\{\}\[\]]/.test(next))
			{
				pairCount++;
			}
		}

		if (pairCount === ranges.length)
		{
			cm.operation(function() {
				cm.replaceSelection(open + close, null);
				cm.triggerElectric(open + close);
				cm.execCommand("goCharLeft");
			});
		}
		else if (surroundCount === ranges.length)
		{
			cm.operation(function() {
				var selections = cm.getSelections();
				for (var i = 0; i < selections.length; i++)
				{
					selections[i] = open + selections[i] + close;
				}
				cm.replaceSelections(selections, "around");
				selections = cm.listSelections().slice();
				for (var i = 0; i < selections.length; i++)
				{
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
		}
		else
		{
			return open == close ? handleClose(close)(cm) : CodeMirror.Pass;
		}
	};
}

function handleClose(close)
{
	return function(cm)
	{
		var ranges = cm.listSelections();
		for (var i = 0; i < ranges.length; i++)
		{
			var range = ranges[i];
			var pos = range.head;
			var next = cm.getRange(pos, CodeMirror.Pos(pos.line, pos.ch + 1));
			if (range.empty() && next == close)
			{
				continue;
			}
			else
			{
				return CodeMirror.Pass;
			}
		}
		cm.execCommand("goCharRight");
	}
}



// RESIZE

dividerNode.addEventListener('mousedown', function(e) {
	editorNode.style.pointerEvents = 'none';
	outputNode.style.pointerEvents = 'none';
	document.body.addEventListener('mouseup', dividerUp);
	document.body.addEventListener('mousemove', dividerMove);
});

function dividerUp()
{
	editorNode.style.pointerEvents = 'auto';
	outputNode.style.pointerEvents = 'auto';
	document.body.removeEventListener('mouseup', dividerUp);
	document.body.removeEventListener('mousemove', dividerMove);
}

function dividerMove(e)
{
	if (e.buttons === 0)
	{
		dividerUp();
		return;
	}

	var fraction = 100 * (e.pageX / window.innerWidth);
	dividerNode.style.left = fraction + '%';
	editorNode.style.width = fraction + '%';
	outputNode.style.width = 100 - fraction + '%';
}



// HINTS
//
// We delay initialization of hints until page is fully loaded otherwise.
// It is fine if hints do not work at first (or even not at all)
// Seems more important for the editor to appear as quickly as possible!
//

var hints = null;
var importEnd = 0;

window.addEventListener('load', function() {
	var script = document.createElement('script');
	script.src = '/assets/editor-hints.js'
	script.addEventListener('load', function()
	{
		hints = Elm.Main.init({
			node: document.getElementsByClassName('hint')[0],
			flags: codeNode.value
		});
		editor.on("cursorActivity", function() {
			hints.ports.cursorMoves.send(getHint(editor));
		});
		hints.ports.importEndLines.subscribe(function(n) { importEnd = n; });
	});
	document.head.appendChild(script);
});



// GET HINT


function getHint(editor)
{
	var start = editor.getCursor('anchor');
	var cursor = editor.getCursor('head');

	if (start.line !== cursor.line || start.ch !== cursor.ch)
	{
		return null;
	}

	var line = cursor.line;
	var token = editor.getTokenAt(cursor);
	var type = token.type;
	return type === 'variable' ? getLowerHint(editor, line, token)
		: type === 'variable-2' ? getUpperHint(editor, line, token)
		: type === 'keyword' ? getKeywordHint(editor, line, token)
		: type === 'def' ? getDefHint(token.string)
		: null;
}



// GET HINTS


function getLowerHint(editor, line, token)
{
	return getPrefix(editor, line, token) + token.string;
}


function getUpperHint(editor, line, token)
{
	var name = getPrefix(editor, line, token) + token.string + getPostfix(editor, line, token);
	if (line < importEnd)
	{
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


function getKeywordHint(editor, line, token)
{
	switch (token.string)
	{
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


function getDefHint(name)
{
	return name === 'init' || name === 'view' || name === 'update' || name === 'subscriptions'
		? 'def:' + name
		: null;
}



// EXPAND VARIABLES


function getPrefix(editor, line, token)
{
	var dot = getTokenBefore(editor, line, token);
	if (dot.string !== '.') { return ''; }

	var qualifier = getTokenBefore(editor, line, dot);
	return qualifier.type === 'variable-2'
		? getPrefix(editor, line, qualifier) + qualifier.string + '.'
		: '.';
}


function getPostfix(editor, line, token)
{
	var dot = getTokenAfter(editor, line, token);
	if (dot.string !== '.') { return ''; }

	var next = getTokenAfter(editor, line, dot);
	return next.type === 'variable'
		? '.' + next.string
		: next.type === 'variable-2'
			? '.' + next.string + getPostfix(editor, line, next)
			: '.';
}


function getTokenBefore(editor, line, token)
{
	return editor.getTokenAt({ line: line, ch: token.start });
}


function getTokenAfter(editor, line, token)
{
	return editor.getTokenAt({ line: line, ch: token.end + 1 });
}

