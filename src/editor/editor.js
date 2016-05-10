var editor;
var refreshImports = function() {};

// COMPILE

function compile()
{
	var form = document.getElementById('inputForm');
	form.submit();
	refreshImports();
}


// SETUP CODEMIRROR

function initEditor()
{
	var controlsDiv = document.getElementById('controls');
	var controls = Elm.EditorControls.embed(controlsDiv);
	controls.ports.compile.subscribe(compile);
	controls.ports.lights.subscribe(toggleTheme);

	editor = CodeMirror.fromTextArea(document.getElementById('input'), {
		lineNumbers: true,
		matchBrackets: true,
		theme: getTheme(),
		tabMode: 'shift',
		extraKeys: {
			'Ctrl-Enter': compile,
			'Tab': function(cm) {
				var spaces = Array(cm.getOption("indentUnit") + 1).join(" ");
				cm.replaceSelection(spaces, "end", "+input");
			}
		}
	});
	if (window.location.pathname.indexOf("Bounce") === -1 || window === window.top)
	{
		editor.focus();
	}
	editor.on('cursorActivity', function() {
		var token = getToken();
		controls.ports.tokens.send(token);
	});
	refreshImports = function() {
		var imports = parseImports();
		controls.ports.rawImports.send(imports);
	};
	refreshImports();
}


// TOKENS

function getToken() {
	var position = editor.getCursor();
	var line = position.line;

	// get the nearest token
	var token = editor.getTokenAt(position);
	if (!token.type)
	{
		token = editor.getTokenAt({ line: line, ch: position.ch + 1 });
	}

	// detect if token is a qualified variable and format it for Elm
	if (token.type === 'variable')
	{
		return expandLeft(line, token.start, token.string);
	}
	if (token.string === '.' || token.type === 'variable-2')
	{
		return expandRight(line, token.end, expandLeft(line, token.start, token.string));
	}
	if (token.type === 'builtin')
	{
		return token.string;
	}
	return null;
}


function expandLeft(line, start, string)
{
	var token = editor.getTokenAt({ line: line, ch: start });
	if (start === token.start)
	{
		return string;
	}
	if (token.string === '.' || token.type === 'variable-2')
	{
		return expandLeft(line, token.start - 1, token.string + string);
	}
	return string;
}


function expandRight(line, end, string)
{
	var token = editor.getTokenAt({ line: line, ch: end + 1 });
	if (end === token.end)
	{
		return string;
	}
	if (token.string === '.' || token.type === 'variable-2')
	{
		return expandRight(line, token.end, string + token.string);
	}
	if (token.type === 'variable')
	{
		return string + token.string;
	}
	return string;
}


// IMPORTS

function parseImports()
{
	var value = editor.doc.getValue();
	var regex = /(?:^|\n)import\s([\w\.]+)(?:\sas\s(\w+))?(?:\sexposing\s*\(((?:\s*(?:\w+|\(.+\))\s*,)*)\s*((?:\.\.|\w+|\(.+\)))\s*\))?/g;

	var imports = [];
	while (match = regex.exec(value))
	{
		var exposedString = match[3] + match[4];
		var exposed = null;
		if (exposedString)
		{
			exposed = exposedString.split(',').map(function(variable) {
				var trimmed = variable.trim();
				return trimmed[0] === '('
					? trimmed.slice(1,-1).trim()
					: trimmed;

			});
		}
		imports.push({
			name: match[1],
			alias: match[2] || null,
			exposed: exposed
		});
	}
	return imports;
}


// THEMES

var THEME_KEY = 'theme'

function getTheme()
{
	return localStorage.getItem(THEME_KEY) || 'mbo';
}

function toggleTheme()
{
	var theme = getTheme();
	var newTheme = theme === 'mbo' ? 'elegant' : 'mbo';
	editor.setOption('theme', newTheme);
	localStorage.setItem(THEME_KEY, newTheme);
}