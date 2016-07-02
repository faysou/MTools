If you're interested in using Couchbase with MTools, go to 
http://developer.couchbase.com/documentation/server/current/introduction/intro.html
and download the Java SDK.
Put the following files (adjusted with the latest version names) in the Java>Couchbase-Java-Client folder of MTools:
couchbase-core-io-1.2.7.jar
couchbase-java-client-2.2.6.jar
rxjava-1.0.17.jar

When debugging with Wolfram Workbench make sure that the debug configuration of MTools.nb
has "Use JLink from Mathematica Installation" checked. This ensures that you can launch Java on parallel kernels.
(Right click on MTools.nb>Debug>DebugConfigurations>Connection>Use JLink from Mathematica Installation)