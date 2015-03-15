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
	controls.outputs.lights.subscribe(function() {
		var theme = editor.getOption('theme');
		editor.setOption('theme', theme === 'mbo' ? 'elegant' : 'mbo');
	});

	editor = CodeMirror.fromTextArea(document.getElementById('input'), {
		lineNumbers: initLines(),
		matchBrackets: true,
		theme: 'mbo',
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

function initLines()
{
	var lines = readCookie('lineNumbers');
	if (lines)
	{
		lines = lines === 'true';
		document.getElementById('editor_lines').checked = lines;
		return lines;
	}
	return false;
}

/*	// return "eclipse"
	// return "neo"
	// return "tomorrow-night-eighties"  ///////
	// return "tomorrow-night-bright"  /////////
	// return "3024-night"
	// return "3024-day"
	return "elegant"
	return "mbo" ///////////
	// return "night"
	// return "twilight"
	// return "ambiance-mobile"
	// return "erlang-dark"
	// return "paraiso-dark" //////////////
	// return "paraiso-light" //////////////
	// return "vibrant-ink"
	// return "ambiance"
	// return "lesser-dark"
	// return "xq-dark" /////////////////
	// return "xq-light"
	// return "base16-dark" //////////
	// return "base16-light" /////////
	// return "pastel-on-dark"
	// return "mdn-like"
	// return "rubyblue"
	// return "zenburn"  ////////
	// return "blackboard"
	// return "midnight"
	// return "solarized"  ////////////
	// return "cobalt"
	// return "monokai"
	// return "the-matrix"
	// return "colorforth"
	// return "neat"
*/

function showLines(on)
{
	editor.setOption('lineNumbers', on);
	cookie('lineNumbers', on);
}

function setZoom()
{
	var editorDiv = document.getElementsByClassName('CodeMirror')[0];
	var classes = editorDiv.getAttribute('class').split(' ');
	var input = document.getElementById('editor_zoom');
	var zoomLevel = input.options[input.selectedIndex].innerHTML;
	var zoom = 'zoom-' + zoomLevel.slice(0,-1);
	var newClasses = [];
	for (var i = classes.length; i--; )
	{
		if (!(classes[i].match(/^zoom-/)))
		{
			newClasses.push(classes[i]);
		}
	}
	newClasses.push(zoom);
	editorDiv.setAttribute('class', newClasses.join(' '));
	editor.refresh();
	cookie('zoom', zoomLevel);
}


// COOKIES

function cookie(name,value)
{
	createCookie(name, value, 5*365);
}

function createCookie(name,value,days)
{
	var expires = "";
	if (days)
	{
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		expires = "; expires=" + date.toGMTString();
	}
	document.cookie = name + "=" + value + expires + "; path=/";
}

function readCookie(name)
{
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++)
	{
		var c = ca[i];
		while (c.charAt(0)==' ')
		{
			c = c.substring(1,c.length);
		}
		if (c.indexOf(nameEQ) === 0)
		{
			return c.substring(nameEQ.length,c.length);
		}
	}
	return null;
}

function eraseCookie(name)
{
	createCookie(name,"",-1);
}