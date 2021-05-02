' Copyright (C) 2010-2012 Thies Gerken

' This file is part of Fractals.

' Fractals is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.

' Fractals is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.

' You should have received a copy of the GNU General Public License
' along with Fractals. If not, see <http://www.gnu.org/licenses/>.

Public Class ActionTraceListener
    Inherits TraceListener

#Region "Delegates"

    Public Delegate Sub UpdateActionDelegate(ByVal message As String)

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the update action.
    ''' </summary>
    ''' <value>The update action.</value>
    Public Property UpdateAction As UpdateActionDelegate

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ActionTraceListener" /> class.
    ''' </summary>
    ''' <param name="action">The action.</param>
    Public Sub New(ByVal action As UpdateActionDelegate)
        Me.UpdateAction = action
    End Sub

#End Region

#Region "Overriden Members"

    ''' <summary>
    ''' When overridden in a derived class, writes the specified message to the listener you create in the derived class.
    ''' </summary>
    ''' <param name="message">A message to write.</param>
    Public Overloads Overrides Sub Write(ByVal message As String)
        WriteLine(message)
    End Sub

    ''' <summary>
    ''' When overridden in a derived class, writes a message to the listener you create in the derived class, followed by a line terminator.
    ''' </summary>
    ''' <param name="message">A message to write.</param>
    Public Overloads Overrides Sub WriteLine(ByVal message As String)
        UpdateAction.Invoke(message)
    End Sub

#End Region
End Class
