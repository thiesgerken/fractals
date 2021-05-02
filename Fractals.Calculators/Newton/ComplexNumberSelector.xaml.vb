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

Imports System.Windows
Imports System.ComponentModel
Imports Fractals.Mathematics

Public Class ComplexNumberSelector
    Implements INotifyPropertyChanged

    Private _realPart As Double
    Private _imaginaryPart As Double

    Public Property RealPart As Double
        Get
            Return _realPart
        End Get
        Set(ByVal value As Double)
            _realPart = value

            OnPropertyChanged("RealPart")
        End Set
    End Property

    Public Property ImaginaryPart As Double
        Get
            Return _imaginaryPart
        End Get
        Set(ByVal value As Double)
            _imaginaryPart = value

            OnPropertyChanged("ImaginaryPart")
        End Set
    End Property

    Public ReadOnly Property Result As ComplexNumber
        Get
            Return New ComplexNumber(RealPart, ImaginaryPart)
        End Get
    End Property

    Private Sub DeleteButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles DeleteButton.Click
        OnRemoveButtonPressed()
    End Sub

    Public Event RemoveButtonPressed(ByVal sender As Object, ByVal e As EventArgs)

    Private Sub OnRemoveButtonPressed()
        RaiseEvent RemoveButtonPressed(Me, New EventArgs)
    End Sub

    Private Shadows Sub OnPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    Public Event PropertyChanged(ByVal sender As Object, ByVal e As PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

    Private Sub ComplexNumberSelector_Initialized(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Initialized
        Me.DataContext = Me
    End Sub
End Class
