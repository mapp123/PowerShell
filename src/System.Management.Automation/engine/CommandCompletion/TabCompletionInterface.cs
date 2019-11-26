
using System.Collections;
using System.Management.Automation.Language;

namespace System.Management.Automation
{
    internal interface TabCompletionInterface
    {
        CommandCompletion CompleteInput(string input, int cursorIndex, Hashtable options);

        CommandCompletion CompleteInput(Ast ast, Token[] tokens, IScriptPosition positionOfCursor, Hashtable options);

        CommandCompletion CompleteInput(string input, int cursorIndex, Hashtable options, PowerShell powershell);

        CommandCompletion CompleteInput(Ast ast, Token[] tokens, IScriptPosition cursorPosition, Hashtable options, PowerShell powershell);

        CommandCompletion CompleteInputInDebugger(string input, int cursorIndex, Hashtable options, Debugger debugger);

        CommandCompletion CompleteInputInDebugger(Ast ast, Token[] tokens, IScriptPosition cursorPosition, Hashtable options, Debugger debugger);
    }
}
