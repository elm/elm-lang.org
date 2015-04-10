var editor;
var refreshImports = function() {};

// COMPILE

function compile()
{
	var form = document.getElementById('inputForm');
	form.submit();
	refreshImports();
}


// HOT SWAP

function hotSwap()
{
	var request = new XMLHttpRequest();
	request.onreadystatechange = function(e) {
		if (request.readyState === 4
			&& request.status >= 200
			&& request.status < 300)
		{
			var result = JSON.parse(request.responseText);
			var top = self.parent;
			if (js = result.success)
			{
				var error = top.output.document.getElementById('ErrorMessage');
				if (error)
				{
					error.parentNode.removeChild(error);
				}
				top.output.eval(js);
				var module = top.output.eval('Elm.' + result.name);
				top.output.runningElmModule = top.output.runningElmModule.swap(module);
				refreshImports();
			}
			else
			{
				var error = top.output.document.getElementById('ErrorMessage');
				if (!error)
				{
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


// SETUP CODEMIRROR

function initEditor()
{
	var controlsDiv = document.getElementById('controls');
	var controls = Elm.embed(Elm.EditorControls, controlsDiv, { rawImports: [], tokens: null });
	controls.ports.compile.subscribe(compile);
	controls.ports.hotSwap.subscribe(hotSwap);
	controls.ports.lights.subscribe(toggleTheme);

	editor = CodeMirror.fromTextArea(document.getElementById('input'), {
		lineNumbers: true,
		matchBrackets: true,
		theme: getTheme(),
		tabMode: 'shift',
		extraKeys: {
			'Ctrl-Enter': compile,
			'Shift-Ctrl-Enter': hotSwap,
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