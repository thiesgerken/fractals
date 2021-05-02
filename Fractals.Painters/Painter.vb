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
Imports System.IO
Imports System.ComponentModel
Imports Cloo
Imports CommandLineParser
Imports Fluent
Imports Fractals.Utilities
Imports System.Xml

''' <summary>
''' Base for the Painters
''' </summary>
Public MustInherit Class Painter
    Implements ICloneable, INotifyPropertyChanged, ICommandLineExtender

#Region "Fields"
    Private _dataSource As DataSource
#End Region

#Region "Properties"


    ''' <summary>
    ''' Gets the name of the Painter.
    ''' </summary>
    ''' <value>The name.</value>
    Public MustOverride ReadOnly Property Name As String

    ''' <summary>
    ''' Gets the description of the Painter.
    ''' </summary>
    ''' <value>The description.</value>
    Public MustOverride ReadOnly Property Description As String

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public MustOverride ReadOnly Property PreviewImage As Bitmap

    ''' <summary>
    ''' Gets a value indicating whether this <see cref="Painter" /> is selectable in the GUI.
    ''' If this retruns <c>false</c>, this painter will only be shown if the current calculator specified it as an additional painter.
    ''' </summary>
    ''' <value><c>true</c> if selectable; otherwise, <c>false</c>.</value>
    Public MustOverride ReadOnly Property Selectable As Boolean

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this painter is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public MustOverride ReadOnly Property RibbonTab As RibbonTabItem

    ''' <summary>
    ''' Gets a value indicating whether this painter uses data provided by datasources.
    ''' </summary>
    ''' <value><c>true</c> if this painter needs data; otherwise, <c>false</c>.</value>
    Public MustOverride ReadOnly Property NeedsData As Boolean

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public MustOverride ReadOnly Property Source As String

    ''' <summary>
    ''' Gets the arguments (name, type).
    ''' </summary>
    ''' <value>The arguments.</value>
    Public MustOverride ReadOnly Property Arguments As Dictionary(Of String, String)

    ''' <summary>
    ''' A PaintDataSource Value determining whether to use n or z.Phi
    ''' </summary>
    ''' <value>The data source.</value>
    Public Property DataSource As DataSource
        Get
            Return _dataSource
        End Get
        Set(ByVal value As DataSource)
            _dataSource = value

            OnPropertyChanged("DataSource")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets infos about the parent fractal.
    ''' </summary>
    ''' <value>The parent info.</value>
    Public Property ParentInfo As FractalInfo

    ''' <summary>
    ''' Gets or sets the type of this painter.
    ''' </summary>
    ''' <value>The type.</value>
    Public Property Type As PainterType

#End Region

#Region "Methods"

    ''' <summary>
    ''' Initializes this instance. Called before any pixels are colored.
    ''' </summary>
    Public Overridable Sub Initialize()
    End Sub

    ''' <summary>
    ''' Makes sure that a given number is in the interval [0;255].
    ''' </summary>
    ''' <param name="b">The number to test.</param>
    ''' <returns>255 if b >= 255, 0 if b is lower than 0, otherwise b</returns>
    Protected Shared Function NormalizeByte(ByVal b As Double) As Integer
        If b < 0 Or Double.IsNaN(b) Or Double.IsInfinity(b) Then Return 0
        If b > 255 Then Return 255

        Return CInt(b)
    End Function

    ''' <summary>
    ''' Saves this instance to a binary stream.
    ''' </summary>
    ''' <param name="x">The xmlwriter to perform the saving on.</param>
    Public Shared Sub Save(ByVal obj As Painter, ByVal x As XmlWriter)
        x.WriteStartElement("Type")
        x.WriteType(obj.GetType)
        x.WriteEndElement()

        x.WriteStartElement("DataSource")
        x.WriteType(obj.DataSource.GetType)
        x.WriteEndElement()

        x.WriteStartElement("Properties")
        obj.SaveParameters(x)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected MustOverride Sub SaveParameters(ByVal x As XmlWriter)

    ''' <summary>
    ''' Loads the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal x As XElement) As Painter
        Dim inst As Painter = CType(Activator.CreateInstance(SaveUtils.LoadType(x.Element("Type"))), Painter)

        inst.DataSource = CType(Activator.CreateInstance(SaveUtils.LoadType(x.Element("DataSource"))), DataSource)
        inst.LoadParameters(x.Element("Properties"))

        Return inst
    End Function

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected MustOverride Sub LoadParameters(ByVal x As XElement)

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="kernel">The kernel.</param>
    ''' <param name="positions">The positions.</param>
    Public MustOverride Sub SetArguments(ByVal kernel As ComputeKernel, ByVal commands As ComputeCommandQueue, ByVal positions As Dictionary(Of String, Integer))

    ''' <summary>
    ''' Creates a new object that is a copy of the current instance.
    ''' </summary>
    ''' <returns>
    ''' A new object that is a copy of this instance.
    ''' </returns>
    Public Function Clone() As Object Implements System.ICloneable.Clone
        Using ms As New MemoryStream
            Using x = XmlWriter.Create(ms)
                x.WriteStartElement("Root")
                Painter.Save(Me, x)
                x.WriteEndElement()
            End Using

            ms.Position = 0

            Return Painter.Load(New XDocument(ms).Element("Root"))
        End Using
    End Function
#End Region

#Region "OnPropertyChanged"

    ''' <summary>
    ''' Called when [property changed].
    ''' </summary>
    ''' <param name="propertyName">Name of the property.</param>
    Protected Sub OnPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    ''' <summary>
    ''' Occurs when a property value changes.
    ''' </summary>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

#End Region

    ''' <summary>
    ''' Gets the arguments.
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overridable ReadOnly Property CommandLineArguments As Argument() Implements ICommandLineExtender.CommandLineArguments
        Get
            Return New Argument() {}
        End Get
    End Property

    ''' <summary>
    ''' Evaluates the arguments.
    ''' </summary>
    Public Overridable Sub EvaluateCommandLineArguments() Implements ICommandLineExtender.EvaluateCommandLineArguments
    End Sub
End Class
