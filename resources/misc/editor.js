var editor;


// COMPILE

function compile()
{
	var form = document.getElementById('inputForm');
	form.submit();
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
	var controls = Elm.embed(Elm.EditorControls, controlsDiv);
	controls.outputs.compile.subscribe(compile);
	controls.outputs.hotSwap.subscribe(hotSwap);
	controls.outputs.lights.subscribe(toggleTheme);

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