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

Imports System.CodeDom.Compiler
Imports System.Text
Imports Microsoft.VisualBasic

''' <summary>
''' Represents a function f(x,y,z,w)
''' </summary>
''' <typeparam name="T1">The type of the 1. argument</typeparam>
''' <typeparam name="T2">The type of the 2. argument</typeparam>
''' <typeparam name="T3">The type of the 3. argument</typeparam>
''' <typeparam name="T4">The type of the 4. argument</typeparam>
''' <typeparam name="TResult">The type of the result.</typeparam>
Public MustInherit Class MathFunction(Of T1, T2, T3, T4, TResult)

#Region "Properties"

    ''' <summary>
    ''' Gets the string representation. (something like x^2 or so)
    ''' </summary>
    ''' <value>The string representation.</value>
    Public MustOverride ReadOnly Property StringRepresentation As String

    ''' <summary>
    ''' Enables the f(x) syntax (one would have to call f.Evaluate(x) without this)
    ''' </summary>
    ''' <value></value>
    Default Public ReadOnly Property Item(ByVal x As T1, ByVal y As T2, ByVal z As T3, ByVal w As T4) As TResult
        Get
            Return Evaluate(x, y, z, w)
        End Get
    End Property

    ''' <summary>
    ''' Enables the f(x) syntax (one would have to call f.Evaluate(x) without this)
    ''' </summary>
    ''' <value></value>
    Default Public ReadOnly Property Item(ByVal x As T1, ByVal y As T2, ByVal z As T3) As TResult
        Get
            Return Evaluate(x, y, z)
        End Get
    End Property

    ''' <summary>
    ''' Enables the f(x) syntax (one would have to call f.Evaluate(x) without this)
    ''' </summary>
    ''' <value></value>
    Default Public ReadOnly Property Item(ByVal x As T1, ByVal y As T2) As TResult
        Get
            Return Evaluate(x, y)
        End Get
    End Property

    ''' <summary>
    ''' Enables the f(x) syntax (one would have to call f.Evaluate(x) without this)
    ''' </summary>
    ''' <value></value>
    Default Public ReadOnly Property Item(ByVal x As T1) As TResult
        Get
            Return Evaluate(x)
        End Get
    End Property

#End Region

#Region "Methods"

    ''' <summary>
    ''' Evaluates the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public MustOverride Function Evaluate(ByVal x As T1, ByVal y As T2, ByVal z As T3, ByVal w As T4) As TResult

    ''' <summary>
    ''' Evaluates the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <param name="z">The z.</param>
    ''' <returns></returns>
    Public Function Evaluate(ByVal x As T1, ByVal y As T2, ByVal z As T3) As TResult
        Return Evaluate(x, y, z, Nothing)
    End Function

    ''' <summary>
    ''' Evaluates the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns></returns>
    Public Function Evaluate(ByVal x As T1, ByVal y As T2) As TResult
        Return Evaluate(x, y, Nothing, Nothing)
    End Function

    ''' <summary>
    ''' Evaluates the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Function Evaluate(ByVal x As T1) As TResult
        Return Evaluate(x, Nothing, Nothing, Nothing)
    End Function

    ''' <summary>
    ''' Creates a MathFunction depending on x from the specified expression.
    ''' </summary>
    ''' <param name="expression">The expression (for f(x) = x^2 this would be x^2)</param>
    ''' <returns></returns>
    Public Shared Function Create(ByVal expression As String) As MathFunction(Of T1, T2, T3, T4, TResult)
        Dim compiler As New VBCodeProvider

        Dim parameters As New CompilerParameters
        parameters.GenerateInMemory = True
        parameters.GenerateExecutable = False
        parameters.ReferencedAssemblies.Add("System.dll")
        parameters.ReferencedAssemblies.Add("Fractals.Utilities.dll")
        parameters.ReferencedAssemblies.Add("Fractals.Mathematics.dll")

        Dim t1Name = GetType(T1).Name
        Dim t2Name = GetType(T2).Name
        Dim t3Name = GetType(T3).Name
        Dim t4Name = GetType(T4).Name
        Dim tResultName = GetType(TResult).Name

        Dim sourceCode As New StringBuilder

        sourceCode.AppendLine("Imports System")
        sourceCode.AppendLine("Imports Fractals.Utilities")
        sourceCode.AppendLine("Imports Fractals.Mathematics")

        sourceCode.AppendLine("Public Class CustomMathFunction")
        sourceCode.AppendLine("Inherits MathFunction(Of " & t1Name & ", " & t2Name & ", " & t3Name & ", " & t4Name & ", " & tResultName & ")")

        sourceCode.AppendLine("Public Overrides ReadOnly Property StringRepresentation As String")
        sourceCode.AppendLine("Get")
        sourceCode.AppendLine("Return """ & expression & """")
        sourceCode.AppendLine("End Get")
        sourceCode.AppendLine("End Property")

        If GetType(TResult) Is GetType(ComplexNumber) Then
            'Todo: check for characters next to the i (could be sin or so, too!)
            expression = expression.Replace("i", " New ComplexNumber(0,1) ")
        End If

        sourceCode.AppendLine("Public Overrides Function Evaluate(ByVal x As " & t1Name & ", ByVal y As " & t2Name & ", ByVal z As " & t3Name & ", ByVal w As " & t4Name & ") As " & tResultName)
        sourceCode.AppendLine("Return " & expression)
        sourceCode.AppendLine("End Function")

        sourceCode.AppendLine("End Class")

        Dim results = compiler.CompileAssemblyFromSource(parameters, sourceCode.ToString)

        For Each ex In results.Errors
            Throw New Exception(ex.ToString)
        Next

        Dim classType = results.CompiledAssembly.GetType("CustomMathFunction")
        Return CType(Activator.CreateInstance(classType), MathFunction(Of T1, T2, T3, T4, TResult))
    End Function

#End Region
End Class
