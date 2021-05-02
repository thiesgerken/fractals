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

Imports Fractals.Mathematics
Imports System.Windows.Media
Imports System.Windows

Public Class FunctionCreatorWindow

    Private _func As String
    Private _derivative As String

    Public ReadOnly Property Func As String
        Get
            Return _func
        End Get
    End Property

    Public ReadOnly Property Derivative As String
        Get
            Return _derivative
        End Get
    End Property

    Private Sub CancelButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles CancelButton.Click
        Me.DialogResult = False
        Me.Close()
    End Sub

    Private Sub AddButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles AddButton.Click
        AddRoot()
    End Sub

    Private Sub AddRoot()
        Dim cms As New ComplexNumberSelector

        AddHandler cms.RemoveButtonPressed, Sub(s, ev)
                                                RootStackPanel.Children.Remove(CType(s, Windows.UIElement))

                                                CreateButton.IsEnabled = Not (RootStackPanel.Children.Count = 1)
                                            End Sub

        RootStackPanel.Children.Insert(RootStackPanel.Children.Count - 1, cms)
        CreateButton.IsEnabled = True
    End Sub

    Private Sub FunctionCreatorWindow_Loaded(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles Me.Loaded
        AddRoot()
        Me.Background = SystemColors.WindowBrush
    End Sub

    Private Sub CreateButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles CreateButton.Click
        Dim roots As New List(Of ComplexNumber)

        For Each element In RootStackPanel.Children
            If element.GetType Is GetType(ComplexNumberSelector) Then
                roots.Add(CType(element, ComplexNumberSelector).Result)
            End If
        Next

        _func = MathFunctions.GetFunction(roots, 1)
        _derivative = MathFunctions.GetDerivative(Func)

        Me.DialogResult = True
        Me.Close()
    End Sub
End Class
