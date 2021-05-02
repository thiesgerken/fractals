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

Imports System.Drawing
Imports Fluent
Imports Fractals.Mathematics

''' <summary>
''' A Painter which gets its colors from math functions that can be set from the user.
''' </summary>
Public Class CustomPainter
    Inherits Painter
    
#Region "Private Fields"

    'f(n,maxN,z)
    Private _rFunc As MathFunction(Of Double, Double)
    Private _gFunc As MathFunction(Of Double, Double)
    Private _bFunc As MathFunction(Of Double, Double)

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="CustomPainter" /> class.
    ''' </summary>
    Public Sub New()
        RFunc = "255*x"
        GFunc = "255*x"
        BFunc = "255*x"
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("R")
        x.WriteValue(RFunc)
        x.WriteEndElement()

        x.WriteStartElement("G")
        x.WriteValue(GFunc)
        x.WriteEndElement()

        x.WriteStartElement("B")
        x.WriteValue(BFunc)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        RFunc = x.Element("R").Value
        GFunc = x.Element("G").Value
        BFunc = x.Element("B").Value
    End Sub

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the R func.
    ''' </summary>
    ''' <value>The R func.</value>
    Public Property RFunc As String
        Get
            Return _rFunc.StringRepresentation
        End Get
        Set(ByVal value As String)
            _rFunc = MathFunction(Of Double, Double).Create(value)

            OnPropertyChanged("RFunc")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the G func.
    ''' </summary>
    ''' <value>The G func.</value>
    Public Property GFunc As String
        Get
            Return _gFunc.StringRepresentation
        End Get
        Set(ByVal value As String)
            _gFunc = MathFunction(Of Double, Double).Create(value)

            OnPropertyChanged("GFunc")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the B func.
    ''' </summary>
    ''' <value>The B func.</value>
    Public Property BFunc As String
        Get
            Return _bFunc.StringRepresentation
        End Get
        Set(ByVal value As String)
            _bFunc = MathFunction(Of Double, Double).Create(value)

            OnPropertyChanged("BFunc")
        End Set
    End Property

    ''' <summary>
    ''' Gets the description of the Painter.
    ''' </summary>
    ''' <value>The description.</value>
    Public Overrides ReadOnly Property Description As String
        Get
            Return ""
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this <see cref="Painter" /> is selectable in the GUI.
    ''' If this retruns <c>false</c>, this painter will only be shown if the current calculator specified it as an additional painter.
    ''' </summary>
    ''' <value><c>true</c> if selectable; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property Selectable As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets the name of the Painter.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Custom"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.CustomPainter
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this painter uses data provided by datasources.
    ''' </summary>
    ''' <value><c>true</c> if this painter needs data; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property NeedsData As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this painter is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public Overrides ReadOnly Property RibbonTab As RibbonTabItem
        Get
            Return Nothing
        End Get
    End Property

#End Region

    Public Overrides ReadOnly Property Source As String
        Get
            Return ""
        End Get
    End Property

    Public Overrides ReadOnly Property Arguments As System.Collections.Generic.Dictionary(Of String, String)
        Get
            Return New Dictionary(Of String, String)
        End Get
    End Property

    Public Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel, ByVal commands As Cloo.ComputeCommandQueue, ByVal positions As System.Collections.Generic.Dictionary(Of String, Integer))

    End Sub
End Class
