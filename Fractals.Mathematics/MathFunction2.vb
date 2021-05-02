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

Public Class MathFunction(Of T1, T2, TResult)

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the inner function.
    ''' </summary>
    ''' <value>The inner.</value>
    Private Property Inner As MathFunction(Of T1, T2, Object, Object, TResult)

    ''' <summary>
    ''' Gets the string representation. (something like x^2 or so)
    ''' </summary>
    ''' <value>The string representation.</value>
    Public ReadOnly Property StringRepresentation As String
        Get
            Return Inner.StringRepresentation
        End Get
    End Property

    ''' <summary>
    ''' Enables the f(x) syntax (one would have to call f.Evaluate(x) without this)
    ''' </summary>
    ''' <value></value>
    Default Public ReadOnly Property Item(ByVal x As T1, ByVal y As T2) As TResult
        Get
            Return Inner(x, y)
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="MathFunction(Of T, TResult)" /> class.
    ''' </summary>
    ''' <param name="inner">The inner.</param>
    Private Sub New(ByVal inner As MathFunction(Of T1, T2, Object, Object, TResult))
        Me.Inner = inner
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Evaluates the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Function Evaluate(ByVal x As T1, ByVal y As T2) As TResult
        Return Inner.Evaluate(x, y)
    End Function

    ''' <summary>
    ''' Creates a MathFunction depending on x from the specified expression.
    ''' </summary>
    ''' <param name="expression">The expression (for f(x) = x^2 this would be x^2)</param>
    ''' <returns></returns>
    Public Shared Function Create(ByVal expression As String) As MathFunction(Of T1, T2, TResult)
        Return New MathFunction(Of T1, T2, TResult)(MathFunction(Of T1, T2, Object, Object, TResult).Create(expression))
    End Function

#End Region
End Class
