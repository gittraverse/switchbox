var code = `package com.hmkcode;


import java.util.Map;
import java.util.TreeMap;

public class MapSort {

  public static Map sortByValue(Map unsortedMap){
  	Map sortedMap = new TreeMap(new ValueComparator(unsortedMap));
  	sortedMap.putAll(unsortedMap);
  	return sortedMap;
  }
  public static Map sortByKey(Map unsortedMap){
  	Map sortedMap = new TreeMap();
  	sortedMap.putAll(unsortedMap);
  	return sortedMap;
  }
}`;
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
console.log(moc);