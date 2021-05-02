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

Imports System.Reflection

Public Module Misc

    ''' <summary>
    ''' Gets every type inheriting BaseType found in a specific assembly.
    ''' </summary>
    ''' <typeparam name="BaseType">the base type.</typeparam>
    ''' <param name="location">The assembly to search in.</param>
    ''' <returns></returns>
    Public Function GetTypes(Of BaseType)(ByVal location As Assembly) As ICollection(Of Type)
        Dim types As New List(Of Type)

        For Each t In GetType(BaseType).Assembly.GetTypes
            If InheritsFrom(t, GetType(BaseType)) Then
                types.Add(t)
            End If
        Next

        Return types
    End Function

    ''' <summary>
    ''' Gets every type inheriting BaseType found in the assembly where BaseType is declared.
    ''' </summary>
    ''' <typeparam name="BaseType">the base type.</typeparam>
    ''' <returns></returns>
    Public Function GetTypes(Of BaseType)() As ICollection(Of Type)
        Return GetTypes(Of BaseType)(GetType(BaseType).Assembly)
    End Function

    ''' <summary>
    ''' Determines whether a type is inheriting from a specified type.
    ''' </summary>
    ''' <param name="t">The t.</param>
    ''' <param name="baseType">Type of the base.</param>
    ''' <returns></returns>
    Public Function InheritsFrom(ByVal t As Type, ByVal baseType As Type) As Boolean
        While Not t.BaseType Is GetType(Object)
            If t.BaseType Is Nothing Then Return False
            If t.BaseType Is baseType Then Return True

            t = t.BaseType
        End While

        Return False
    End Function
End Module
