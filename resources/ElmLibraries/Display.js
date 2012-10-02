
var toArray = function(elmList) {
    var a = [];
    while (elmList.datatype === "Cons") {
	a.push(elmList.data[0]);
	elmList = elmList.data[1];
    }
    return a;
};

var newElement = function(elementType) {
    var e = document.createElement(elementType);    
    e.id = Guid.guid();
    return e;
};

var toDom = function(element) {
    switch (element.datatype) {
    case "EText": {
	var e = newElement('div');
	e.innerHTML = toArray(element.data[0]);
	e.style.textAlign = 'left';
	return e;
    }
    case "EImage": {
	var img = new Image();
	img.name = toArray(element.data[0]);
	img.src = img.name;
	return img;
    }
    case "ECollection":
    break;
    }
};