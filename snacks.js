var finderJava = function(code){
  codeArray = code.split("\n");
  var classMatch = /public class \w+/;
  var methodMatch = /(public|private) (static)? \w+ \w+/;
  var moc = [];
  for(var i = 0; i < codeArray.length; i++)
  {
  	if(codeArray[i].match(classMatch)){
  		var segment = codeArray[i].match(classMatch);
  		var curClass = segment[0].replace(/public class /,"");
  		moc.push({line: i, name: curClass, type: "class"})
  	}
  	if(codeArray[i].match(methodMatch)){
  		var segment = codeArray[i].match(methodMatch);
  		var curMethod = segment[0].replace(/(public|private) (static)? \w+ /,"");
  		moc.push({line: i, name: curMethod, type: "method"})
  	}
  }
  return moc;
}

var finderJavascript = function(code) {
  return null;
};

var finder = function(extension){
  return {
    '.java': finderJava,
    '.js', finderJavascript,
  }[extension];
}

module.exports = {
  finder
}
