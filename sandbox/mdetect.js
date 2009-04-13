
/* *******************************************
// LICENSE INFORMATION
// The code, "Detecting Smartphones Using JavaScript" 
// by Anthony Hand, is licensed under a Creative Commons 
// Attribution 3.0 United States License. 
//
// Anthony Hand, ahand@hand-interactive.com
// Web: www.hand-interactive.com
// 
// License info: http://creativecommons.org/licenses/by/3.0/us/
//
// This code is provided “AS IS” with no expressed or implied warranty.  
// You have the right to use this code or any portion of it 
// so long as you provide credit toward myself as the original author.
//
// *******************************************
*/

//Initialize some initial string variables we'll look for later.
var deviceIphone = "iphone";
var deviceIpod = "ipod";
var devicePlaystation = "playstation";
var deviceWap = "wap";

var deviceWinMob = "windows ce";
var enginePie = "wm5 pie";
var deviceIeMob = "iemobile";

var deviceS60 = "series60";
var deviceSymbian = "symbian";
var deviceS60 = "series60";
var deviceS70 = "series70";
var deviceS80 = "series80";
var deviceS90 = "series90";

var deviceBB = "blackberry";

var deviceAndroid = "android";

var deviceMidp = "midp";
var deviceWml = "wml";
var deviceBrew = "brew";

var devicePalm = "palm";
var engineXiino = "xiino";
var engineBlazer = "blazer"; //Old Palm

var devicePda = "pda";
var deviceNintendoDs = "nitro";

var engineWebKit = "webkit";
var engineNetfront = "netfront";


var manuSonyEricsson = "sonyericsson";
var manuericsson = "ericsson";
var manuSamsung1 = "sec-sgh";

var svcDocomo = "docomo";
var svcKddi = "kddi";
var svcVodafone = "vodafone";

//Due to the flexibility of the S60 OSSO Browser, 
//   you may wish to let new S60 devices get the regular pages instead.
var s60GetsMobile = true;


//Due to the flexibility of the iPhone/iPod Touch Browser, 
//   you may wish to let new S60 devices get the regular pages instead.
var iphoneIpodGetsMobile = true;


//Initialize our user agent string.
var uagent = navigator.userAgent.toLowerCase();


//**************************
// Detects if the current device is an iPhone.
function DetectIphone()
{
   if (uagent.search(deviceIphone) > -1)
   {
      //The iPod touch says it's an iPhone! So let's disambiguate.
      if (uagent.search(deviceIpod) > -1)
         return false;
      else 
         return true;
   }
   else
      return false;
}

//**************************
// Detects if the current device is an iPod Touch.
function DetectIpod()
{
   if (uagent.search(deviceIpod) > -1)
      return true;
   else
      return false;
}

//**************************
// Detects if the current device is an iPhone or iPod Touch.
function DetectIphoneOrIpod()
{
   //We repeat the searches here because some iPods 
   //  may report themselves as an iPhone, which is ok.
   if (uagent.search(deviceIphone) > -1 ||
       uagent.search(deviceIpod) > -1)
       return true;
    else
       return false;
}

//**************************
// Detects if the current device is an Android OS-based device.
function DetectAndroid()
{
   if (uagent.search(deviceAndroid) > -1)
      return true;
   else
      return false;
}


//**************************
// Detects if the current device is an Android OS-based device and
//   the browser is based on WebKit.
function DetectAndroidWebKit()
{
   if (DetectAndroid())
   {
     if (DetectWebkit())
        return true;
     else
        return false;
   }
   else
      return false;
}

//**************************
// Detects if the current browser is based on WebKit.
function DetectWebkit()
{
   if (uagent.search(engineWebKit) > -1)
      return true;
   else
      return false;
}

//**************************
// Detects if the current browser is the Nokia S60 Open Source Browser.
function DetectS60OssBrowser()
{
   if (DetectWebkit())
   {
     if ((uagent.search(deviceS60) > -1 || 
          uagent.search(deviceSymbian) > -1))
        return true;
     else
        return false;
   }
   else
      return false;
}

//**************************
// Detects if the current device is any Symbian OS-based device,
//   including older S60, Series 70, Series 80, Series 90, and UIQ, 
//   or other browsers running on these devices.
function DetectSymbianOS()
{
   if (uagent.search(deviceSymbian) > -1 ||
       uagent.search(deviceS60) > -1 ||
       uagent.search(deviceS70) > -1 ||
       uagent.search(deviceS80) > -1 ||
       uagent.search(deviceS90) > -1)
      return true;
   else
      return false;
}


//**************************
// Detects if the current browser is a BlackBerry of some sort.
function DetectBlackBerry()
{
   if (uagent.search(deviceBB) > -1)
      return true;
   else
      return false;
}

//**************************
// Detects if the current browser is a Windows Mobile device.
function DetectWindowsMobile()
{
   //Most devices use 'Windows CE', but some report 'iemobile' 
   //  and some older ones report as 'PIE' for Pocket IE. 
   if (uagent.search(deviceWinMob) > -1 ||
       uagent.search(deviceIeMob) > -1 ||
       uagent.search(enginePie) > -1)
      return true;
   else
      return false;
}

//**************************
// Detects if the current browser is on a PalmOS device.
function DetectPalmOS()
{
   //Most devices nowadays report as 'Palm', 
   //  but some older ones reported as Blazer or Xiino.
   if (uagent.search(devicePalm) > -1 ||
       uagent.search(engineBlazer) > -1 ||
       uagent.search(engineXiino) > -1)
      return true;
   else
      return false;
}

//**************************
// Sets whether S60 devices running the 
//   Open Source Browser (based on WebKit)
//   should be detected as 'mobile' or not.
//   Set TRUE to be detected as mobile.
//   Set FALSE and it will not be detected as mobile.
function SetS60GetsMobile(setMobile)
{
   s60GetsMobile = setMobile;
};

//**************************
// Sets whether iPhone/iPod Touch devices running the 
//   Open Source Browser (based on WebKit)
//   should be detected as 'mobile' or not.
//   Set TRUE to be detected as mobile.
//   Set FALSE and it will not be detected as mobile.
function SetS60GetsMobile(setMobile)
{
   iphoneIpodGetsMobile = setMobile;
};


//**************************
// Check to see whether the device is a 'smartphone'.
//   You might wish to send smartphones to a more capable web page
//   than a dumbed down WAP page. 
function DetectSmartphone()
{
   //First, look for iPhone and iPod Touch.
   if (DetectIphoneOrIpod())
      return true;

   //Now, look for S60 Open Source Browser on S60 release 3 devices.
   if (DetectS60OssBrowser())
      return true;

   //Check for other Symbian devices - older S60, UIQ, other.
   if (DetectSymbianOS())
      return true;

   //Check for Windows Mobile devices.
   if (DetectWindowsMobile())
      return true;

   //Next, look for a BlackBerry
   if (DetectBlackBerry())
      return true;

   //PalmOS.
   if (DetectPalmOS())
      return true;

   //Otherwise, return false.
   return false;
};


//**************************
// Detects if the current device is a mobile device.
//  This method catches most of the popular modern devices.
function DetectMobileQuick()
{
   //Attempt to detect most mobile devices, 
   //   especially mass market feature phones.
   // NOTE: Doesn't usually work reliably...
   if (uagent.search(deviceWap) > -1   || 
	uagent.search(deviceMidp) > -1 ||
	uagent.search(deviceWml) > -1  ||
	uagent.search(deviceBrew) > -1  )
   {
      return true;
   }

   //Detect for most smartphones.
   if (DetectSmartphone())
      return true;

   //Check for a NetFront browser
   if (uagent.search(engineNetfront) > -1)
      return true;

   //Check for a Playstation
   if (uagent.search(devicePlaystation) > -1)
      return true;

   //Check for a generic PDA
   if (uagent.search(devicePda) > -1)
      return true;

   return false;
};


//**************************
// Detects in a more comprehensive way if the current device is a mobile device.
function DetectMobileLonger()
{
   //Run the quick check first.
   if (DetectMobileQuick())
      return true;

   //Check for NTT Docomo
   if (uagent.search(svcDocomo) > -1)
      return true;

   //Check for KDDI
   if (uagent.search(svcKddi) > -1)
      return true;

   //Check for Nintendo DS
   if (uagent.search(deviceNintendoDs) > -1)
      return true;

   //Check for Vodafone 3G
   if (uagent.search(svcVodafone) > -1)
      return true;

   //Finally, detect for certain very old devices with stupid useragent strings.
   if (uagent.search(manuSamsung1) > -1 ||
	uagent.search(manuSonyEricsson) > -1 || 
	uagent.search(manuericsson) > -1)
   {
      return true;
   }

   return false;
};


