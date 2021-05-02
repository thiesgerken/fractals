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

Imports System.ComponentModel
Imports Fractals.Calculators
Imports System.Threading

Public Class ExportTab
    Implements INotifyPropertyChanged

    Private _userChangedSize As Boolean
    Private _selectedWidth As Integer
    Private _selectedHeight As Integer
    Private WithEvents _target As Fractal = New Multibrot
    Private _hasToRender As Boolean

    ''' <summary>
    ''' Gets or sets the target.
    ''' </summary>
    ''' <value>The target.</value>
    Public Property Target As Fractal
        Get
            Return _target
        End Get
        Set(ByVal value As Fractal)
            _target = value

            RefreshUI()
            OnPropertyChanged("Target")
        End Set
    End Property


    ''' <summary>
    ''' Gets or sets the width of the selected.
    ''' </summary>
    ''' <value>The width of the selected.</value>
    Public Property SelectedWidth As Integer
        Get
            Return _selectedWidth
        End Get
        Set(ByVal value As Integer)
            _selectedWidth = value

            If _selectedWidth <> Target.Size.Width Then
                _userChangedSize = True
            End If

            RefreshUI()

            OnPropertyChanged("SelectedWidth")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the height of the selected.
    ''' </summary>
    ''' <value>The height of the selected.</value>
    Public Property SelectedHeight As Integer
        Get
            Return _selectedHeight
        End Get
        Set(ByVal value As Integer)
            _selectedHeight = value

            If _selectedHeight <> Target.Size.Height Then
                _userChangedSize = True
            End If

            RefreshUI()

            OnPropertyChanged("SelectedHeight")
        End Set
    End Property

    ''' <summary>
    ''' Refreshes the UI.
    ''' </summary>
    Private Sub RefreshUI()
        Me.Dispatcher.Invoke(Sub()
                                 If Target.NeedsRecalculation Or New System.Drawing.Size(SelectedWidth, SelectedHeight) <> Target.Size Then
                                     _hasToRender = True
                                     RenderButton.Content = "Render"

                                     RecalculationTextBlock.Visibility = Windows.Visibility.Visible
                                 Else
                                     _hasToRender = False
                                     RenderButton.Content = "Export"

                                     RecalculationTextBlock.Visibility = Windows.Visibility.Hidden
                                 End If
                             End Sub)
    End Sub

    ''' <summary>
    ''' Handles the Initialized event of the ExportTab control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.EventArgs" /> instance containing the event data.</param>
    Private Sub ExportTab_Initialized(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Initialized
        MainGrid.DataContext = Me
    End Sub

    ''' <summary>
    ''' Handles the Loaded event of the ExportTab control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub ExportTab_Loaded(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles Me.Loaded
        _userChangedSize = False

        SelectedWidth = Target.Size.Width
        SelectedHeight = Target.Size.Height
    End Sub

    ''' <summary>
    ''' Called when [property changed].
    ''' </summary>
    ''' <param name="propertyName">Name of the property.</param>
    Private Shadows Sub OnPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    ''' <summary>
    ''' Occurs when a property value changes.
    ''' </summary>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

    Private _renderThread As Thread

    Private Sub RenderButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles RenderButton.Click
        _renderThread = New Thread(Sub()
                                       Dim oldTarget = Target

                                       If _hasToRender Then
                                           Target = CType(Target.Clone, Fractal)

                                           Me.Dispatcher.Invoke(Sub()
                                                                    WidthTextBox.IsEnabled = False
                                                                    HeightTextBox.IsEnabled = False
                                                                End Sub)

                                           Target.Size = New System.Drawing.Size(SelectedWidth, SelectedHeight)
                                           Target.StartRendering()
                                           Target.Join()
                                       End If

                                       If Not Target.Image Is Nothing Then
                                           Dim filename = "E:\Downloaded\fra.bmp"

                                           If IO.File.Exists(filename) Then IO.File.Delete(filename)

                                           Target.Image.Save(filename)
                                       End If

                                       Me.Dispatcher.Invoke(Sub()
                                                                WidthTextBox.IsEnabled = True
                                                                HeightTextBox.IsEnabled = True
                                                            End Sub)
                                       Target = oldTarget
                                   End Sub)

        _renderThread.Start()
    End Sub

    ''' <summary>
    ''' Handles the PropertyChanged event of the _target control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.ComponentModel.PropertyChangedEventArgs" /> instance containing the event data.</param>
    Private Sub _target_PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Handles _target.PropertyChanged
        If Not _userChangedSize Then
            SelectedWidth = Target.Size.Width
            SelectedHeight = Target.Size.Height
        End If

        RefreshUI()
    End Sub
End Class
