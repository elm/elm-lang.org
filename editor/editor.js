
function lights() {
	editor.setOption('theme', editor.getOption('theme') === 'dark' ? 'light' : 'dark');
}

function compile() {
	document.getElementById('code').value = editor.getValue();
	document.getElementById('editor').submit();
}

var editor = CodeMirror.fromTextArea(document.getElementById('code'), {
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
