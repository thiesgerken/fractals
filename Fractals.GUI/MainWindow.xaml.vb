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

Imports Fractals.Calculators
Imports Fractals.Painters
Imports Fractals.Utilities
Imports Fractals.Mathematics
Imports Fluent
Imports System
Imports System.ComponentModel
Imports System.Reflection

''' <summary>
''' Application's Main Window.
''' </summary>
Class MainWindow
    Implements INotifyPropertyChanged

    Public BuildRev As String = ""
    Public BuildDate As String = ""
    Public BuildTime As String = ""

#Region "Commands"

    Public Shared RefreshCommand As New RoutedCommand
    Public Shared StopCommand As New RoutedCommand
    Public Shared IncreaseZoomCommand As New RoutedCommand
    Public Shared DecreaseZoomCommand As New RoutedCommand
    Public Shared ResetZoomCommand As New RoutedCommand
    Public Shared MoveLeftCommand As New RoutedCommand
    Public Shared MoveRightCommand As New RoutedCommand
    Public Shared MoveUpCommand As New RoutedCommand
    Public Shared MoveDownCommand As New RoutedCommand

#End Region

#Region "Private Fields"

    Private WithEvents _fractal As Fractal
    Private _handle As IntPtr
    Private _fileName As String

    Private _pauseIcon16 As New BitmapImage(New Uri("pack://application:,,,/Images/pause16.png"))
    Private _pauseIcon24 As New BitmapImage(New Uri("pack://application:,,,/Images/pause24.png"))
    Private _pauseIcon32 As New BitmapImage(New Uri("pack://application:,,,/Images/pause32.png"))

    Private _startIcon16 As New BitmapImage(New Uri("pack://application:,,,/Images/start16.png"))
    Private _startIcon24 As New BitmapImage(New Uri("pack://application:,,,/Images/start24.png"))
    Private _startIcon32 As New BitmapImage(New Uri("pack://application:,,,/Images/start32.png"))

    Private _currentFractalPreviewImage As BitmapSource
    Private _currentFractalTooltipImage As BitmapSource
    Private _currentFractalTab As RibbonTabItem

    Private _currentInsidePainterPreviewImage As BitmapSource
    Private _currentInsidePainterTooltipImage As BitmapSource
    Private _currentInsidePainterTab As RibbonTabItem

    Private _currentOutsidePainterPreviewImage As BitmapSource
    Private _currentOutsidePainterTooltipImage As BitmapSource
    Private _currentOutsidePainterTab As RibbonTabItem

    Private _fractalTypes As ICollection(Of GalleryItem)
    Private _insidePainterTypes As ICollection(Of GalleryItem)
    Private _outsidePainterTypes As ICollection(Of GalleryItem)

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the name of the file.
    ''' </summary>
    ''' <value>The name of the file.</value>
    Public Property FileName As String
        Get
            Return _fileName
        End Get
        Set(ByVal value As String)
            _fileName = value
            OnPropertyChanged("FileName")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the memory meter.
    ''' </summary>
    ''' <value>The memory meter.</value>
    Public Property MemoryMeter As New MemoryMeter(Of IterationResult)

    ''' <summary>
    ''' Gets or sets the fractal.
    ''' </summary>
    ''' <value>The fractal.</value>
    Public Property Fractal As Fractal
        Get
            Return _fractal
        End Get
        Set(ByVal value As Fractal)
            _fractal = value

            CurrentFractalPreviewImage = _fractal.PreviewImage.Resize(32, 32).ToBitmapSource
            CurrentFractalTooltipImage = _fractal.PreviewImage.Resize(150, 150).ToBitmapSource

            CurrentInsidePainterPreviewImage = _fractal.InsidePainter.PreviewImage.Resize(32, 32).ToBitmapSource
            CurrentInsidePainterTooltipImage = _fractal.InsidePainter.PreviewImage.Resize(150, 150).ToBitmapSource

            CurrentOutsidePainterPreviewImage = _fractal.OutsidePainter.PreviewImage.Resize(32, 32).ToBitmapSource
            CurrentOutsidePainterTooltipImage = _fractal.OutsidePainter.PreviewImage.Resize(150, 150).ToBitmapSource

            ExportBackstageItem.DataContext = _fractal
            MainGrid.DataContext = _fractal
            RefreshImageSize()

            '0 = first run
            If My.Settings.ThreadCount = 0 Then My.Settings.ThreadCount = Math.Max(Environment.ProcessorCount - 1, 1)

            _insidePainterTypes = Nothing
            _outsidePainterTypes = Nothing

            OnPropertyChanged("OutsidePainterTypes")
            OnPropertyChanged("InsidePainterTypes")
            OnPropertyChanged("Fractal")

            RefreshFractalTab()
            RefreshInsidePainterTab()
            RefreshOutsidePainterTab()

            ExportBackstageItem.Target = _fractal
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the current fractal preview image.
    ''' </summary>
    ''' <value>The current fractal preview image.</value>
    Public Property CurrentFractalPreviewImage As BitmapSource
        Get
            Return _currentFractalPreviewImage
        End Get
        Set(ByVal value As BitmapSource)
            _currentFractalPreviewImage = value

            OnPropertyChanged("CurrentFractalPreviewImage")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the current fractal tooltip image.
    ''' </summary>
    ''' <value>The current fractal tooltip image.</value>
    Public Property CurrentFractalTooltipImage As BitmapSource
        Get
            Return _currentFractalTooltipImage
        End Get
        Set(ByVal value As BitmapSource)
            _currentFractalTooltipImage = value

            OnPropertyChanged("CurrentFractalTooltipImage")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the current inside painter preview image.
    ''' </summary>
    ''' <value>The current inside painter preview image.</value>
    Public Property CurrentInsidePainterPreviewImage As BitmapSource
        Get
            Return _currentInsidePainterPreviewImage
        End Get
        Set(ByVal value As BitmapSource)
            _currentInsidePainterPreviewImage = value

            OnPropertyChanged("CurrentInsidePainterPreviewImage")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the current inside painter tooltip image.
    ''' </summary>
    ''' <value>The current inside painter tooltip image.</value>
    Public Property CurrentInsidePainterTooltipImage As BitmapSource
        Get
            Return _currentInsidePainterTooltipImage
        End Get
        Set(ByVal value As BitmapSource)
            _currentInsidePainterTooltipImage = value

            OnPropertyChanged("CurrentInsidePainterTooltipImage")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the current outside painter preview image.
    ''' </summary>
    ''' <value>The current outside painter preview image.</value>
    Public Property CurrentOutsidePainterPreviewImage As BitmapSource
        Get
            Return _currentOutsidePainterPreviewImage
        End Get
        Set(ByVal value As BitmapSource)
            _currentOutsidePainterPreviewImage = value

            OnPropertyChanged("CurrentOutsidePainterPreviewImage")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the current outside painter tooltip image.
    ''' </summary>
    ''' <value>The current outside painter tooltip image.</value>
    Public Property CurrentOutsidePainterTooltipImage As BitmapSource
        Get
            Return _currentOutsidePainterTooltipImage
        End Get
        Set(ByVal value As BitmapSource)
            _currentOutsidePainterTooltipImage = value

            OnPropertyChanged("CurrentOutsidePainterTooltipImage")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether [auto refresh].
    ''' </summary>
    ''' <value><c>true</c> if [auto refresh]; otherwise, <c>false</c>.</value>
    Public Property AutoRefresh As Boolean
        Get
            Return My.Settings.AutoRefresh
        End Get
        Set(ByVal value As Boolean)
            My.Settings.AutoRefresh = value
            My.Settings.Save()
        End Set
    End Property

    ''' <summary>
    ''' Gets a list of all fractal types found.
    ''' </summary>
    ''' <value>The fractal types.</value>
    Public ReadOnly Property FractalTypes As ICollection(Of GalleryItem)
        Get
            If _fractalTypes Is Nothing Then
                Dim items As New List(Of GalleryItem)

                For Each fractalType In GetTypes(Of Fractal)()
                    Dim instance As Fractal = CType(Activator.CreateInstance(fractalType), Fractal)
                    Dim item As New GalleryItem
                    item.Content = New Image With {.Source = instance.PreviewImage.Resize(70, 70).ToBitmapSource, .Stretch = Stretch.None, .ToolTip = New ScreenTip With {.Title = instance.Name, .Text = instance.Description, .Image = instance.PreviewImage.Resize(150, 150).ToBitmapSource}}

                    item.Tag = fractalType
                    items.Add(item)

                    instance.Dispose()
                Next

                _fractalTypes = items
            End If

            Return _fractalTypes
        End Get
    End Property

    ''' <summary>
    ''' Gets a list of all inside painters types found.
    ''' </summary>
    ''' <value>The painter types.</value>
    Public ReadOnly Property InsidePainterTypes As ICollection(Of GalleryItem)
        Get
            If _insidePainterTypes Is Nothing Then
                _insidePainterTypes = FindPainters(PainterType.Interior)
            End If

            Return _insidePainterTypes
        End Get
    End Property

    ''' <summary>
    ''' Gets a list of all outside painters types found.
    ''' </summary>
    ''' <value>The painter types.</value>
    Public ReadOnly Property OutsidePainterTypes As ICollection(Of GalleryItem)
        Get
            If _outsidePainterTypes Is Nothing Then
                _outsidePainterTypes = FindPainters(PainterType.Exterior)
            End If

            Return _outsidePainterTypes
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="MainWindow" /> class.
    ''' </summary>
    Public Sub New()
        ' This call is required by the designer.
        InitializeComponent()

        ' other Initializations
        Me.DataContext = Me

        Dim args = Environment.GetCommandLineArgs
        Dim file = ""

        If args.Length > 1 Then
            If IO.File.Exists(args(1)) Then file = args(1)
        End If

        If file <> "" Then
            FileName = file

            Try
                Fractal = Fractal.Load(file)
            Catch ex As Exception
                Fractal = New Multibrot
            End Try
        Else
            Fractal = New Multibrot
        End If
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Refreshes the outside painter tab.
    ''' </summary>
    Private Sub RefreshOutsidePainterTab()
        'delete the old tab from ribbon if there was one
        If _currentOutsidePainterTab IsNot Nothing Then
            _currentOutsidePainterTab.Group = Nothing
            Ribbon.Tabs.Remove(_currentOutsidePainterTab)
        End If

        _currentOutsidePainterTab = _fractal.OutsidePainter.RibbonTab

        If _currentOutsidePainterTab IsNot Nothing Or _fractal.OutsidePainter.NeedsData Then
            If _currentOutsidePainterTab Is Nothing Then _currentOutsidePainterTab = New RibbonTabItem

            'force name
            _currentOutsidePainterTab.Header = _fractal.OutsidePainter.Name

            If _fractal.OutsidePainter.NeedsData Then
                'add datasource selector
                _currentOutsidePainterTab.Groups.Insert(0, CreateDataSourceSelector(_fractal.OutsidePainter))
            End If

            'add it to ribbon and associate it to the contextual group
            Ribbon.Tabs.Add(_currentOutsidePainterTab)
            _currentOutsidePainterTab.Group = OutsidePainterGroup
        End If
    End Sub

    ''' <summary>
    ''' Refreshes the inside painter tab.
    ''' </summary>
    Private Sub RefreshInsidePainterTab()
        'delete the old tab from ribbon if there was one
        If _currentInsidePainterTab IsNot Nothing Then
            _currentInsidePainterTab.Group = Nothing
            Ribbon.Tabs.Remove(_currentInsidePainterTab)
        End If

        _currentInsidePainterTab = _fractal.InsidePainter.RibbonTab

        If _currentInsidePainterTab IsNot Nothing Or _fractal.InsidePainter.NeedsData Then
            If _currentInsidePainterTab Is Nothing Then _currentInsidePainterTab = New RibbonTabItem

            'force name
            _currentInsidePainterTab.Header = _fractal.InsidePainter.Name

            If _fractal.InsidePainter.NeedsData Then
                'add datasource selector
                _currentInsidePainterTab.Groups.Insert(0, CreateDataSourceSelector(_fractal.InsidePainter))
            End If

            'add it to ribbon and associate it to the contextual group
            Ribbon.Tabs.Add(_currentInsidePainterTab)
            _currentInsidePainterTab.Group = InsidePainterGroup
        End If
    End Sub

    ''' <summary>
    ''' Refreshes the fractal tab.
    ''' </summary>
    Private Sub RefreshFractalTab()
        'delete the old tab from ribbon if there was one
        If _currentFractalTab IsNot Nothing Then
            _currentFractalTab.Group = Nothing
            Ribbon.Tabs.Remove(_currentFractalTab)
        End If

        _currentFractalTab = _fractal.RibbonTab

        'fractals may return nothing if they do not want to display any settings
        If _currentFractalTab Is Nothing Then _currentFractalTab = New RibbonTabItem

        'force name
        _currentFractalTab.Header = _fractal.Name

        'add common fractal settings box
        Dim settingsGroupBox As New RibbonGroupBox
        settingsGroupBox.Header = "Common Fractal Settings"
        settingsGroupBox.DataContext = _fractal

        Dim iterTextBox As New TextBox
        iterTextBox.InputWidth = 50
        iterTextBox.Header = "IterationLimit: "
        iterTextBox.ToolTip = New ScreenTip() With {.Title = "Maximum Iteration Count", .Text = "Sets the maximum count of iterations that are calculated for a pixel." & Environment.NewLine & "If the pixel does not diverge/converge until this point," & Environment.NewLine & "then it is assumed that it belongs to the fractal.", .DisableReason = "The iteration count is automatically adjusted." & Environment.NewLine & "To change it manually, untick the checkbox 'Auto Adjust' first."}
        iterTextBox.SetBinding(TextBox.TextProperty, "MaxIterations")
        iterTextBox.SetBinding(TextBox.IsEnabledProperty, New Binding("AutoAdjustIterations") With {.Converter = New NotConverter})
        settingsGroupBox.Items.Add(iterTextBox)

        Dim bailoutTextBox As New TextBox
        bailoutTextBox.InputWidth = 50
        bailoutTextBox.Header = "Bailout: "
        bailoutTextBox.ToolTip = New ScreenTip() With {.Title = "Bailout Value", .Text = "After each iteration has to be checked whether the pixel diverged," & Environment.NewLine & "and this value is the comparison to accomplish this." & Environment.NewLine & "If the iteration value exceeds the bailout," & Environment.NewLine & "then the pixel does not belong to the fractal."}
        bailoutTextBox.SetBinding(TextBox.TextProperty, "BailoutValue")
        settingsGroupBox.Items.Add(bailoutTextBox)

        Dim samplesTextBox As New TextBox
        samplesTextBox.InputWidth = 50
        samplesTextBox.Header = "Samples: "
        samplesTextBox.ToolTip = New ScreenTip() With {.Title = "Samples per pixel", .Text = "The count of samples to calculate per pixel." & Environment.NewLine & "Setting a higher count results in a higher image quality," & Environment.NewLine & "but will also increase the calculation time dramatically." & Environment.NewLine & "Please note that the sample count has to be quadratic," & Environment.NewLine & "so enter only value like 'n x n' or a single value which is a n^2.", .DisableReason = "The current fractal type does not support anti-aliasing."}
        samplesTextBox.SetBinding(TextBox.TextProperty, New Binding("AntiAliasingSamples") With {.Converter = New SamplesConverter})
        samplesTextBox.SetBinding(TextBox.IsEnabledProperty, "SupportsAntiAliasing")
        settingsGroupBox.Items.Add(samplesTextBox)

        Dim autoAdjustCheckBox As New CheckBox
        autoAdjustCheckBox.Header = "Auto Adjust"
        autoAdjustCheckBox.SetBinding(CheckBox.IsCheckedProperty, "AutoAdjustIterations")
        autoAdjustCheckBox.ToolTip = New ScreenTip() With {.Title = "Automatic Adjustment of Maximum Iteration Count", .Text = "When activated, the maximum iteration count is chosen" & Environment.NewLine & "automatically based on the current zoom factor."}
        settingsGroupBox.Items.Add(autoAdjustCheckBox)

        Dim smoothingCheckBox As New CheckBox
        smoothingCheckBox.Header = "Smooth"
        smoothingCheckBox.SetBinding(CheckBox.IsCheckedProperty, "SmoothIterationCounts")
        smoothingCheckBox.SetBinding(CheckBox.IsEnabledProperty, "SupportsSmoothing")
        smoothingCheckBox.ToolTip = New ScreenTip() With {.Title = "Smooth Iteration Counts", .Text = "When activated, the iteration is count (integer value)" & Environment.NewLine & "for each pixel is interpolated to achieve more smooth gradients.", .DisableReason = "The fractal does not support smoothing."}
        settingsGroupBox.Items.Add(smoothingCheckBox)

        _currentFractalTab.Groups.Insert(0, settingsGroupBox)

        'add it to ribbon and associate it to the contextual group
        Ribbon.Tabs.Add(_currentFractalTab)
        _currentFractalTab.Group = FractalGroup
    End Sub

    ''' <summary>
    ''' Saves the specified save as.
    ''' </summary>
    ''' <param name="saveAs">if set to <c>true</c> [save as].</param>
    Private Sub Save(ByVal saveAs As Boolean)
        Dim file As String = FileName

        If file = "" Or saveAs Then
            'Show Save File Dialog
            Using sdlg As New Forms.SaveFileDialog
                sdlg.Filter = "Fractal Settings File|*.fractal"
                sdlg.AddExtension = True
                sdlg.CheckPathExists = True
                sdlg.DefaultExt = ".fractal"
                sdlg.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
                sdlg.OverwritePrompt = True
                sdlg.FileName = If(FileName = "", "Untitled", IO.Path.GetFileNameWithoutExtension(FileName))

                If sdlg.ShowDialog = Forms.DialogResult.OK Then
                    file = sdlg.FileName
                Else
                    Return
                End If
            End Using
        End If

        'save
        Fractal.Save(_fractal, file)

        'eventually save filename
        If Not saveAs Then FileName = file
    End Sub

    ''' <summary>
    ''' Opens a fractal from a file.
    ''' </summary>
    Private Sub OpenFromFile()
        Using odlg As New Forms.OpenFileDialog
            odlg.Filter = "Fractal Settings File|*.fractal"
            odlg.AddExtension = True
            odlg.CheckPathExists = True
            odlg.CheckFileExists = True
            odlg.DefaultExt = ".fractal"
            odlg.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)

            If odlg.ShowDialog = Forms.DialogResult.OK Then
                If _fractal.State = WorkingState.Working Then _fractal.AbortRendering()

                FileName = odlg.FileName
                Fractal = Fractal.Load(odlg.FileName)
                FractalImage.Source = BitmapImage.Create(2, 2, 96, 96, PixelFormats.Indexed1, New BitmapPalette(New Color() {Colors.Transparent}), New Byte() {0, 0, 0, 0}, 1)

                If AutoRefresh Then _fractal.StartRendering()
            End If
        End Using
    End Sub

    ''' <summary>
    ''' Creates the data source selector.
    ''' </summary>
    ''' <returns></returns>
    Private Function CreateDataSourceSelector(ByVal boundPainter As Painter) As RibbonGroupBox
        Dim box As New RibbonGroupBox
        box.Header = "Common Painter Settings"

        Dim sourcesComboBox As New ComboBox
        sourcesComboBox.InputWidth = 140
        sourcesComboBox.SetBinding(ComboBox.ToolTipProperty, "ToolTip")
        sourcesComboBox.Header = "DataSource: "
        sourcesComboBox.IsReadOnly = True
        sourcesComboBox.DataContext = Nothing

        For Each t In GetType(DataSource).Assembly.GetTypes()
            If InheritsFrom(t, GetType(DataSource)) And Not t Is GetType(DefaultDataSource) Then
                Dim inst = CType(Activator.CreateInstance(t), DataSource)
                Dim cbox As New ComboBoxItem With {.Content = inst.Name, .Tag = inst, .ToolTip = New ScreenTip With {.Title = inst.Name, .Text = inst.Description}}
                sourcesComboBox.Items.Add(cbox)
                If boundPainter.DataSource.GetType() Is t Then sourcesComboBox.SelectedItem = cbox

                AddHandler cbox.Selected, Sub(s, e)
                                              sourcesComboBox.DataContext = s
                                              boundPainter.DataSource = CType(Activator.CreateInstance(CType(s, ComboBoxItem).Tag.GetType), DataSource)

                                              If AutoRefresh And _fractal.State = WorkingState.None Then _fractal.StartRendering()
                                          End Sub
            End If
        Next

        box.Items.Add(sourcesComboBox)

        Return box
    End Function

    ''' <summary>
    ''' Finds the painters.
    ''' </summary>
    ''' <returns></returns>
    Private Function FindPainters(ByVal type As PainterType) As ICollection(Of GalleryItem)
        Dim items As New List(Of GalleryItem)

        For Each painterType In GetTypes(Of Painter)()
            Dim instance As Painter = CType(Activator.CreateInstance(painterType), Painter)

            If instance.Selectable Or (type = Painters.PainterType.Interior And Fractal.AdditionalInsidePainters.Contains(painterType)) Or (type = Painters.PainterType.Exterior And Fractal.AdditionalOutsidePainters.Contains(painterType)) Then
                Dim item As New GalleryItem
                item.Content = New Image With {.Source = instance.PreviewImage.Resize(70, 70).ToBitmapSource, .Stretch = Stretch.None, .ToolTip = New ScreenTip With {.Title = instance.Name, .Text = instance.Description, .Image = instance.PreviewImage.Resize(150, 150).ToBitmapSource}}
                item.Tag = painterType
                items.Add(item)
            End If
        Next

        Return items
    End Function
    
    ''' <summary>
    ''' Refreshes the size of the image.
    ''' </summary>
    Private Sub RefreshImageSize()
        Dim newSize As New Drawing.Size(CInt(FractalImage.ActualWidth), CInt(FractalImage.ActualHeight))
        If newSize <> _fractal.Size And Not newSize.IsEmpty Then
            'change of size is dangerous if fractal is busy
            If _fractal.State = WorkingState.Working Or _fractal.State = WorkingState.Paused Then _fractal.AbortRendering()

            _fractal.Size = newSize
        End If
    End Sub

    ''' <summary>
    ''' Refreshes the start stop buttons.
    ''' </summary>
    Private Sub RefreshStartStopButtons()
        StopButton.IsEnabled = _fractal.State = WorkingState.Working Or _fractal.State = WorkingState.Paused

        If _fractal.State = WorkingState.Paused Then
            StartPauseResumeButton.Header = "Resume"
            StartPauseResumeButtonScreenTip.Title = "Resume"
            StartPauseResumeButtonScreenTip.Text = "Resumes the rendering process."
            StartPauseResumeButtonScreenTip.Image = _startIcon24
            StartPauseResumeButton.LargeIcon = _startIcon32
            StartPauseResumeButton.Icon = _startIcon16

            ETAItem.Visibility = Windows.Visibility.Visible
            ETASeparator.Visibility = Windows.Visibility.Visible
        ElseIf _fractal.State = WorkingState.Working Then
            StartPauseResumeButton.Header = "Pause"
            StartPauseResumeButtonScreenTip.Title = "Pause"
            StartPauseResumeButtonScreenTip.Text = "Pauses the rendering process."
            StartPauseResumeButtonScreenTip.Image = _pauseIcon24
            StartPauseResumeButton.LargeIcon = _pauseIcon32
            StartPauseResumeButton.Icon = _pauseIcon16

            ETAItem.Visibility = Windows.Visibility.Visible
            ETASeparator.Visibility = Windows.Visibility.Visible
        Else
            StartPauseResumeButton.Header = "Start"
            StartPauseResumeButtonScreenTip.Title = "Start (F5)"
            StartPauseResumeButtonScreenTip.Text = "Starts the rendering process."
            StartPauseResumeButtonScreenTip.Image = _startIcon24
            StartPauseResumeButton.LargeIcon = _startIcon32
            StartPauseResumeButton.Icon = _startIcon16

            ETAItem.Visibility = Windows.Visibility.Collapsed
            ETASeparator.Visibility = Windows.Visibility.Collapsed
        End If
    End Sub

    ''' <summary>
    ''' Handles the Click event of the StartPauseResumeButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub StartPauseResumeButton_Click(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles StartPauseResumeButton.Click
        If _fractal.State = WorkingState.Paused Then
            _fractal.ResumeRendering()
        ElseIf _fractal.State = WorkingState.Working Then
            _fractal.SuspendRendering()
        Else
            _fractal.StartRendering()
        End If
    End Sub

#End Region

#Region "Event Handlers"

#Region "Window"

    ''' <summary>
    ''' Handles the Closing event of the MainWindow control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.ComponentModel.CancelEventArgs" /> instance containing the event data.</param>
    Private Sub MainWindow_Closing(ByVal sender As Object, ByVal e As CancelEventArgs) Handles Me.Closing
        _MemoryMeter.Dispose()
    End Sub

    ''' <summary>
    ''' Handles the Loaded event of the MainWindow control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub MainWindow_Loaded(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles Me.Loaded
        _handle = New Interop.WindowInteropHelper(Me).Handle

        FractalImage.Source = BitmapImage.Create(2, 2, 96, 96, PixelFormats.Indexed1, New BitmapPalette(New Color() {Colors.Transparent}), New Byte() {0, 0, 0, 0}, 1)

        RefreshImageSize()
        RefreshStartStopButtons()
    End Sub

    ''' <summary>
    ''' Handles the SizeChanged event of the MainWindow control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.SizeChangedEventArgs" /> instance containing the event data.</param>
    Private Sub MainWindow_SizeChanged(ByVal sender As Object, ByVal e As SizeChangedEventArgs) Handles Me.SizeChanged
        RefreshImageSize()
    End Sub

    ''' <summary>
    ''' Handles the StateChanged event of the MainWindow control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.EventArgs" /> instance containing the event data.</param>
    Private Sub MainWindow_StateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.StateChanged
        RefreshImageSize()
    End Sub

#End Region

#Region "Fractal"

    ''' <summary>
    ''' Handles the FractalSettingChanged event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.ComponentModel.PropertyChangedEventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_FractalSettingChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Handles _fractal.FractalSettingChanged
        If Me.AutoRefresh And _fractal.State = WorkingState.None Then _fractal.StartRendering()
    End Sub

    ''' <summary>
    ''' Handles the PropertyChanged event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.ComponentModel.PropertyChangedEventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_PropertyChanged(ByVal sender As Object, ByVal e As PropertyChangedEventArgs) Handles _fractal.PropertyChanged
        If e.PropertyName = "InsidePainter" Then
            CurrentInsidePainterPreviewImage = _fractal.InsidePainter.PreviewImage.Resize(32, 32).ToBitmapSource
            CurrentInsidePainterTooltipImage = _fractal.InsidePainter.PreviewImage.Resize(100, 100).ToBitmapSource

            RefreshInsidePainterTab()
            RefreshOutsidePainterTab()
        ElseIf e.PropertyName = "OutsidePainter" Then
            CurrentOutsidePainterPreviewImage = _fractal.OutsidePainter.PreviewImage.Resize(32, 32).ToBitmapSource
            CurrentOutsidePainterTooltipImage = _fractal.OutsidePainter.PreviewImage.Resize(100, 100).ToBitmapSource

            RefreshInsidePainterTab()
            RefreshOutsidePainterTab()
        End If
    End Sub

    ''' <summary>
    ''' Handles the MessageLogged event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="Fractals.Utilities.MessageLoggedEventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_MessageLogged(ByVal sender As Object, ByVal e As MessageLoggedEventArgs) Handles _fractal.MessageLogged
        LogBox.Dispatcher.Invoke(Sub()
                                     LogBox.AppendText(e.Message & Environment.NewLine)
                                     LogBox.ScrollToEnd()
                                 End Sub)
    End Sub

    ''' <summary>
    ''' Handles the RenderingAborted event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.EventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_RenderingAborted(ByVal sender As Object, ByVal e As System.EventArgs) Handles _fractal.RenderingAborted
        Me.Dispatcher.Invoke(Sub() RefreshStartStopButtons())
    End Sub

    ''' <summary>
    ''' Handles the RenderingCompleted event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="EventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_RenderingCompleted(ByVal sender As Object, ByVal e As EventArgs) Handles _fractal.RenderingCompleted
        FractalImage.Dispatcher.Invoke(Sub()
                                           FractalImage.Source = _fractal.Image.ToBitmapSource

                                           RefreshStartStopButtons()
                                           Me.InvalidateVisual()
                                       End Sub)
    End Sub

    ''' <summary>
    ''' Handles the RenderingSuspended event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.EventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_RenderingSuspended(ByVal sender As Object, ByVal e As System.EventArgs) Handles _fractal.RenderingSuspended
        Me.Dispatcher.Invoke(Sub() RefreshStartStopButtons())
    End Sub

    ''' <summary>
    ''' Handles the RenderingStarted event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.EventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_RenderingStarted(ByVal sender As Object, ByVal e As System.EventArgs) Handles _fractal.RenderingStarted
        Me.Dispatcher.Invoke(Sub() RefreshStartStopButtons())
    End Sub

    ''' <summary>
    ''' Handles the ZoomStateChanged event of the _fractal control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.EventArgs" /> instance containing the event data.</param>
    Private Sub _fractal_ZoomStateChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles _fractal.ZoomStateChanged
        _fractal.StartRendering()
    End Sub

#End Region

#Region "FractalImage"

    ''' <summary>
    ''' Handles the MouseLeave event of the FractalImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub FractalImage_MouseLeave(ByVal sender As Object, ByVal e As System.Windows.Input.MouseEventArgs) Handles FractalImage.MouseLeave
        PositionLabel.Visibility = Windows.Visibility.Collapsed
        PositionSeparator.Visibility = Windows.Visibility.Collapsed
    End Sub

    ''' <summary>
    ''' Handles the MouseMove event of the FractalImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseEventArgs" /> instance containing the event data.</param>
    Private Sub FractalImage_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Input.MouseEventArgs) Handles FractalImage.MouseMove
        PositionLabel.Visibility = Windows.Visibility.Visible
        PositionSeparator.Visibility = Windows.Visibility.Visible

        Dim dist = ComplexNumber.Abs(_fractal.ScreenToViewport(1, 1) - _fractal.ScreenToViewport(2, 2))
        Dim digits As Integer

        If dist = 0 Then
            digits = 15
        Else
            digits = CInt(3 - Math.Log10(dist))
        End If

        PositionLabel.Text = _fractal.ScreenToViewport(e.GetPosition(FractalImage)).ToString(digits)
    End Sub

    ''' <summary>
    ''' Handles the MouseRightButtonUp event of the FractalImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseButtonEventArgs" /> instance containing the event data.</param>
    Private Sub FractalImage_MouseRightButtonUp(ByVal sender As Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles FractalImage.MouseRightButtonUp
        If _fractal.CanReset Then _fractal.UndoZoom()
    End Sub

    ''' <summary>
    ''' Handles the MouseWheel event of the FractalImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseWheelEventArgs" /> instance containing the event data.</param>
    Private Sub FractalImage_MouseWheel(ByVal sender As Object, ByVal e As System.Windows.Input.MouseWheelEventArgs) Handles FractalImage.MouseWheel
        Dim f As Double

        If e.Delta < 0 Then
            f = 0.5
        Else
            f = 2
        End If

        _fractal.Zoom(e.GetPosition(FractalImage), f)
    End Sub

    ''' <summary>
    ''' Handles the RubberBandCreated event of the FractalImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="Fractals.Utilities.RubberBandCreatedEventArgs" /> instance containing the event data.</param>
    Private Sub FractalImage_RubberBandCreated(ByVal sender As Object, ByVal e As Utilities.RubberBandCreatedEventArgs) Handles FractalImage.RubberBandCreated
        _fractal.Zoom(e.Region, e.RotationAngle)
    End Sub

    ''' <summary>
    ''' Handles the SizeChanged event of the FractalImage control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.SizeChangedEventArgs" /> instance containing the event data.</param>
    Private Sub FractalImage_SizeChanged(ByVal sender As Object, ByVal e As System.Windows.SizeChangedEventArgs) Handles FractalImage.SizeChanged
        RefreshImageSize()
    End Sub

#End Region

#Region "Other"

    ''' <summary>
    ''' Handles the SizeChanged event of the Ribbon control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.SizeChangedEventArgs" /> instance containing the event data.</param>
    Private Sub Ribbon_SizeChanged(ByVal sender As Object, ByVal e As SizeChangedEventArgs) Handles Ribbon.SizeChanged
        RefreshImageSize()
    End Sub

    ''' <summary>
    ''' Handles the MouseDoubleClick event of the LogLabelItem control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.MouseButtonEventArgs" /> instance containing the event data.</param>
    Private Sub LogLabelItem_MouseDoubleClick(ByVal sender As Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles LogLabelItem.MouseDoubleClick
        If LogBox.Visibility = Windows.Visibility.Visible Then
            LogBox.Visibility = Windows.Visibility.Collapsed
        Else
            LogBox.Visibility = Windows.Visibility.Visible
        End If
    End Sub

    ''' <summary>
    ''' Handles the Click event of the ResetButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub ResetZoomButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles ResetZoomButton.Click
        _fractal.ResetZoom()
    End Sub

    ''' <summary>
    ''' Handles the Click event of the UndoZoomButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub UndoZoomButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles UndoZoomButton.Click
        _fractal.UndoZoom()
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the RefreshCommand control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub RefreshBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles RefreshBinding.Executed
        If _fractal.State = WorkingState.None Then
            _fractal.StartRendering()
        End If
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the StopBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub StopBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles StopBinding.Executed
        If Not _fractal.State = WorkingState.None Then
            _fractal.AbortRendering()
        End If
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the DecreaseZoomBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub DecreaseZoomBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles DecreaseZoomBinding.Executed
        _fractal.Zoom(0.5)
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the IncreaseZoomBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub IncreaseZoomBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles IncreaseZoomBinding.Executed
        _fractal.Zoom(2)
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the ResetZoomBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub ResetZoomBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles ResetZoomBinding.Executed
        _fractal.ResetZoom()
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the MoveLeftBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub MoveLeftBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles MoveLeftBinding.Executed
        Dim rect As New System.Drawing.RectangleF
        rect.X = CSng(_fractal.Size.Width * -0.2)
        rect.Y = 0
        rect.Size = _fractal.Size

        _fractal.Zoom(rect, 0)
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the MoveRightBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub MoveRightBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles MoveRightBinding.Executed
        Dim rect As New System.Drawing.RectangleF
        rect.X = CSng(_fractal.Size.Width * 0.2)
        rect.Y = 0
        rect.Size = _fractal.Size

        _fractal.Zoom(rect, 0)
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the MoveUpBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub MoveUpBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles MoveUpBinding.Executed
        Dim rect As New System.Drawing.RectangleF
        rect.Y = CSng(_fractal.Size.Height * -0.2)
        rect.X = 0
        rect.Size = _fractal.Size

        _fractal.Zoom(rect, 0)
    End Sub

    ''' <summary>
    ''' Handles the Executed event of the MoveDownBinding control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Input.ExecutedRoutedEventArgs" /> instance containing the event data.</param>
    Private Sub MoveDownBinding_Executed(ByVal sender As Object, ByVal e As System.Windows.Input.ExecutedRoutedEventArgs) Handles MoveDownBinding.Executed
        Dim rect As New System.Drawing.RectangleF
        rect.Y = CSng(_fractal.Size.Height * 0.2)
        rect.X = 0
        rect.Size = _fractal.Size

        _fractal.Zoom(rect, 0)
    End Sub

    ''' <summary>
    ''' Handles the SelectionChanged event of the FractalTypesGallery control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Controls.SelectionChangedEventArgs" /> instance containing the event data.</param>
    Private Sub FractalTypesGallery_SelectionChanged(ByVal sender As Object, ByVal e As System.Windows.Controls.SelectionChangedEventArgs) Handles FractalTypesGallery.SelectionChanged
        Dim newType As Type = CType(CType(e.AddedItems(0), GalleryItem).Tag, Type)
        If newType.FullName = _fractal.GetType.FullName Then Return

        Fractal = CType(Activator.CreateInstance(newType), Fractal)
        If AutoRefresh Then _fractal.StartRendering()
    End Sub

    ''' <summary>
    ''' Handles the SelectionChanged event of the OutsidePainterTypesGallery control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Controls.SelectionChangedEventArgs" /> instance containing the event data.</param>
    Private Sub OutsidePainterTypesGallery_SelectionChanged(ByVal sender As Object, ByVal e As System.Windows.Controls.SelectionChangedEventArgs) Handles OutsidePainterTypesGallery.SelectionChanged
        Dim newType As Type = CType(CType(e.AddedItems(0), GalleryItem).Tag, Type)
        If newType.FullName = _fractal.OutsidePainter.GetType.FullName Then Return

        _fractal.OutsidePainter = CType(Activator.CreateInstance(newType), Painter)
    End Sub

    ''' <summary>
    ''' Handles the SelectionChanged event of the InsidePainterTypesGallery control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.Controls.SelectionChangedEventArgs" /> instance containing the event data.</param>
    Private Sub InsidePainterTypesGallery_SelectionChanged(ByVal sender As Object, ByVal e As System.Windows.Controls.SelectionChangedEventArgs) Handles InsidePainterTypesGallery.SelectionChanged
        Dim newType As Type = CType(CType(e.AddedItems(0), GalleryItem).Tag, Type)
        If newType.FullName = _fractal.InsidePainter.GetType.FullName Then Return

        _fractal.InsidePainter = CType(Activator.CreateInstance(newType), Painter)
    End Sub

    ''' <summary>
    ''' Handles the Click event of the ExitButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub ExitButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles ExitButton.Click
        If _fractal.State = WorkingState.Working Then _fractal.AbortRendering()

        Me.Close()
    End Sub

    ''' <summary>
    ''' Handles the Click event of the NewButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub NewButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles NewButton.Click
        RibbonBackstage.IsOpen = False

        If _fractal.State = WorkingState.Working Then _fractal.AbortRendering()

        FileName = ""
        Fractal = New Multibrot
        FractalImage.Source = BitmapImage.Create(2, 2, 96, 96, PixelFormats.Indexed1, New BitmapPalette(New Color() {Colors.Transparent}), New Byte() {0, 0, 0, 0}, 1)
        If AutoRefresh Then _fractal.StartRendering()
    End Sub

    ''' <summary>
    ''' Handles the Click event of the SaveButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub SaveButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles SaveButton.Click
        Save(False)
        RibbonBackstage.IsOpen = False
    End Sub

    ''' <summary>
    ''' Handles the Click event of the SaveAsButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub SaveAsButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles SaveAsButton.Click
        Save(True)
        RibbonBackstage.IsOpen = False
    End Sub

    ''' <summary>
    ''' Handles the Click event of the OpenButton control.
    ''' </summary>
    ''' <param name="sender">The source of the event.</param>
    ''' <param name="e">The <see cref="System.Windows.RoutedEventArgs" /> instance containing the event data.</param>
    Private Sub OpenButton_Click(ByVal sender As Object, ByVal e As System.Windows.RoutedEventArgs) Handles OpenButton.Click
        OpenFromFile()
        RibbonBackstage.IsOpen = False
    End Sub

#End Region

#End Region

#Region "INotifyPropertyChanged Implementation"

    ''' <summary>
    ''' Called when [property changed].
    ''' </summary>
    ''' <param name="propertyName">Name of the property.</param>
    Private Overloads Sub OnPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    ''' <summary>
    ''' Occurs when a property value changes.
    ''' </summary>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

#End Region

End Class
