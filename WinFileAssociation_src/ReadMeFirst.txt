“WinFileAssociation” ReadMe.
============================

“WinFileAssociation” is a very simple application to set Microsoft Windows file associations. Its primary use is to create new
associations, but it may also be used to change existing ones. Beside the application, associated with a given file type (file
extension), the user can choose the icon and description, that should be displayed in File Manager for such files.

“WinFileAssociation” is free of charge, the program is open source; feel free to modify the code, following your needs and
preferences. “WinFileAssociation” is delivered as is, i.e. without any warranty of any kind. Thus, using it, means using it AT
YOUR OWN RISKS!

The application associates a given extension .myext with a given application myapp. Enter the extension and push the “Check” button;
the application tells you if this extension has already an associated program, or if it hasn’t yet resp. isn’t even yet registered.
Any changes in the “File extension” edit field disable the “Create” button. You’ll always have to check the extension before writing
to the registry (pushing “Check” will enable the “Create” button): a certain protection against unwanted changes.

The only mandatory fields are the application name, executable (default application to open this type files) and parameters (automatically
set to “%1”); the extension icon (displayed in File Manager for this file type) should always be set, too. “File description” is the
description, that will be displayed for this file type in File Manager; if it is left blank “MYEXT file” will be used.

“WinFileAssociation” creates two registry keys. The first one “.myext”, with default value set to the content of the “New association”
field. This field is filled in automatically and set to “myapp.FileAssoc.MYEXT”. This will be the name of the second registry key
created. The subkeys of this key will contain all values and data, defining the actual file association. The action to take (when
double-clicking a file with this extension) is always set to “Open”; with the application parameters set to “%1” this means, “run the
application myapp and let it open the file, that was double-clicked”.

If the registry key .myext exists (if this file extension is registered), it will be overwritten. This is also true for the second key.
Thus, be careful when you enter your own data in the “New association” field: Choosing a bad key name here could override a registry
entry other than the one, you want (the application doesn’t actually any checking concerning this possibility)! Also note, that if there
is already a (second) file association key and the key, that you will create now has a different name, the old key will actually not be
removed from the registry.

The advanced settings (access to the “New association” and “App parameters” fields) should only be enabled by advanced users and if you
really need to do so. This is, for example, the case, if the associated application needs other parameters than the name of the file to
be opened. Advanced settings enabled, also allows you to choose the name of the second registry key, even though “WinFileAssociation”
actually accepts only key names of the format “myapp.FileAssoc.mysomething”, where myapp must correspond to your entry in the “App name”
field (with all spaces removed) and mysomething may be any text without spaces. Thus, you may for example create the new file association
myapp.FileAssoc.myapp_data_file (instead of myapp.FileAssoc.MYEXT). Note, that in this case, the file description field (if left blank)
will be set to “myapp data file” (instead of “MYEXT file”).

Finally note, that if you try to create a file association for all users, you may get an error message, that the operation failed. In this
case, quit “WinFileAssociation” and rerun it as Administrator.

                                                                           “WinFileAssociation”, version 1.0.1, © allu, January-August 2021.
																					