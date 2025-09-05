set wsc = CreateObject("WScript.Shell")
DO
	'Five minutes
	Wscript.sleep(5*60*1000)
	wsc.SendKeys("{F13}")
Loop