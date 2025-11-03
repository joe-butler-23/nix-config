{
  pkgs,
  lib,
  ...
}: {
  programs.walker = {
    enable = true;
    runAsService = true;

    config = {
      theme = "modern";
      placeholders."default" = {
        input = "Search";
        list = "Applications";
      };
      providers.prefixes = [
        {provider = "websearch"; prefix = "+";}
        {provider = "providerlist"; prefix = "_";}
      ];
      keybinds.quick_activate = ["F1" "F2" "F3"];
    };

    themes = {
      "modern" = {
        style = ''
          /* AUTO GENERATED. DO NOT EDIT. CHANGES WILL BE OVERWRITTEN. */

          @define-color foreground rgba(255, 255, 255, 0.9);
          @define-color background rgba(30, 30, 46, 0.95);
          @define-color accent hsl(261, 83%, 58%);
          @define-color surface rgba(49, 50, 68, 0.8);
          @define-color border rgba(255, 255, 255, 0.1);
          /* AUTO GENERATED. DO NOT EDIT. CHANGES WILL BE OVERWRITTEN. */

          #window,
          #box,
          #aiScroll,
          #aiList,
          #search,
          #password,
          #input,
          #prompt,
          #clear,
          #typeahead,
          #list,
          child,
          scrollbar,
          slider,
          #item,
          #text,
          #label,
          #bar,
          #sub,
          #activationlabel {
            all: unset;
          }

          #cfgerr {
            background: rgba(255, 0, 0, 0.4);
            margin-top: 20px;
            padding: 8px;
            font-size: 1.2em;
          }

          #window {
            color: @foreground;
          }

          #box {
            border-radius: 12px;
            background: @background;
            padding: 24px;
            border: 1px solid @border;
            box-shadow: 0 20px 40px rgba(0, 0, 0, 0.4);
          }

          #search {
            border-radius: 8px;
            background: @surface;
            padding: 12px 16px;
            border: 1px solid @border;
            margin-bottom: 16px;
          }

          #prompt {
            margin-right: 12px;
            color: @accent;
            opacity: 0.8;
          }

          #clear {
            color: @foreground;
            opacity: 0.8;
          }

          #password,
          #input,
          #typeahead {
            border-radius: 2px;
          }

          #input {
            background: none;
          }

          #password {
          }

          #spinner {
            padding: 8px;
          }

          #typeahead {
            color: @foreground;
            opacity: 0.8;
          }

          #input placeholder {
            opacity: 0.5;
          }

          #list {
          }

          child {
            padding: 8px;
            border-radius: 2px;
          }

          child:selected,
          child:hover {
            background: alpha(@accent, 0.2);
            border-left: 3px solid @accent;
          }

          #item {
          }

          #icon {
            margin-right: 8px;
          }

          #text {
          }

          #label {
            font-weight: 500;
          }

          #sub {
            opacity: 0.5;
            font-size: 0.8em;
          }

          #activationlabel {
          }

          #bar {
          }

          .barentry {
          }

          .activation #activationlabel {
          }

          .activation #text,
          .activation #icon,
          .activation #search {
            opacity: 0.5;
          }

          .aiItem {
            padding: 10px;
            border-radius: 2px;
            color: @foreground;
            background: @background;
          }

          .aiItem.user {
            padding-left: 0;
            padding-right: 0;
          }

          .aiItem.assistant {
            background: lighter(@background);
          }
        '';

        layouts = {
          "window" = ''
            <object class="GtkWindow" id="window">
              <property name="width-request">600</property>
              <property name="resizable">False</property>
              <property name="decorated">False</property>
              <property name="default-width">600</property>
              <child>
                <object class="GtkBox" id="box">
                  <property name="orientation">vertical</property>
                  <property name="spacing">0</property>
                  <property name="margin-top">200</property>
                  <property name="margin-start">24</property>
                  <property name="margin-end">24</property>
                  <property name="margin-bottom">24</property>
                  <child>
                    <object class="GtkBox" id="search">
                      <property name="orientation">horizontal</property>
                      <property name="spacing">12</property>
                      <child>
                        <object class="GtkImage" id="prompt">
                          <property name="icon-name">edit-find</property>
                          <property name="icon-size">large</property>
                          <property name="pixel-size">18</property>
                        </object>
                      </child>
                      <child>
                        <object class="GtkEntry" id="input">
                          <property name="placeholder-text">Search</property>
                          <property name="hexpand">True</property>
                        </object>
                      </child>
                      <child>
                        <object class="GtkButton" id="clear">
                          <property name="icon-name">edit-clear</property>
                          <property name="icon-size">large</property>
                          <property name="pixel-size">18</property>
                        </object>
                      </child>
                    </object>
                  </child>
                  <child>
                    <object class="GtkScrolledWindow" id="scroll">
                      <property name="vscrollbar-policy">automatic</property>
                      <property name="hscrollbar-policy">never</property>
                      <property name="min-content-height">200</property>
                      <property name="max-content-height">400</property>
                      <property name="min-content-width">600</property>
                      <property name="max-content-width">600</property>
                      <property name="margin-top">8</property>
                      <child>
                        <object class="GtkListBox" id="list">
                          <property name="selection-mode">single</property>
                          <style>
                            <class name="list"/>
                          </style>
                        </object>
                      </child>
                    </object>
                  </child>
                </object>
              </child>
            </object>
          '';

          "item" = ''
            <object class="GtkListBoxRow" id="item">
              <property name="selectable">True</property>
              <child>
                <object class="GtkBox" id="box">
                  <property name="orientation">horizontal</property>
                  <property name="spacing">8</property>
                  <property name="margin-start">8</property>
                  <property name="margin-end">8</property>
                  <property name="margin-top">8</property>
                  <property name="margin-bottom">8</property>
                  <child>
                    <object class="GtkImage" id="icon">
                      <property name="pixel-size">26</property>
                    </object>
                  </child>
                  <child>
                    <object class="GtkBox" id="text">
                      <property name="orientation">vertical</property>
                      <property name="spacing">2</property>
                      <property name="hexpand">True</property>
                      <child>
                        <object class="GtkLabel" id="label">
                          <property name="halign">start</property>
                          <property name="wrap">True</property>
                          <property name="wrap-mode">word-char</property>
                        </object>
                      </child>
                      <child>
                        <object class="GtkLabel" id="sub">
                          <property name="halign">start</property>
                          <property name="wrap">True</property>
                          <property name="wrap-mode">word-char</property>
                        </object>
                      </child>
                    </object>
                  </child>
                  <child>
                    <object class="GtkLabel" id="activationlabel">
                      <property name="halign">center</property>
                      <property name="valign">center</property>
                      <property name="width-request">20</property>
                    </object>
                  </child>
                </object>
              </child>
            </object>
          '';

          "ai_scroll" = ''
            <object class="GtkScrolledWindow" id="aiScroll">
              <property name="vscrollbar-policy">automatic</property>
              <property name="hscrollbar-policy">never</property>
              <property name="min-content-height">300</property>
              <property name="max-content-height">300</property>
              <property name="min-content-width">400</property>
              <property name="max-content-width">400</property>
              <property name="margin-top">8</property>
              <child>
                <object class="GtkListBox" id="aiList">
                  <property name="selection-mode">none</property>
                  <property name="orientation">vertical</property>
                  <property name="spacing">10</property>
                </object>
              </child>
            </object>
          '';

          "ai_item" = ''
            <object class="GtkBox" id="aiItem">
              <property name="orientation">vertical</property>
              <property name="spacing">0</property>
              <property name="margin-start">10</property>
              <property name="margin-end">10</property>
              <property name="margin-top">10</property>
              <property name="margin-bottom">10</property>
              <child>
                <object class="GtkLabel" id="text">
                  <property name="halign">start</property>
                  <property name="valign">start</property>
                  <property name="wrap">True</property>
                  <property name="wrap-mode">word-char</property>
                  <property name="xalign">0</property>
                  <property name="yalign">0</property>
                </object>
              </child>
            </object>
          '';
        };
      };
    };
  };
}
