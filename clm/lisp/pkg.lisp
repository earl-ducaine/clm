;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:user; Base:10 -*-

(in-package :cl-user)


(defpackage :xtk
 (:use common-lisp)
  (:nicknames clm)
  (:export
    motif-version
    ld
    add-callback
    clm-error
    :remove-callback
    :remove-all-callbacks
    :has-callbacks
    :add-protocol-callback
    :add-wm-protocol-callback
    :remove-protocol-callback
    :remove-wm-protocol-callback
    :center-at
    :quit-application
    :popup-exclusive
    :popup-nonexclusive
    :popup-none
    :manage-popup
    :unmanage-popup
    :popdown-shell
    :get-x-colors
    :create-arrow-button
    :create-arrow-button-gadget
    :create-bulletin-board
    :create-bulletin-board-dialog
    :create-cascade-button
    :create-cascade-button-gadget
    :create-command
    :create-dialog-shell
    :create-drawing-area
    :create-drawn-button
    :create-error-dialog
    :create-file-selection-box
    :create-file-selection-dialog
    :create-form
    :create-form-dialog
    :create-frame
    :create-information-dialog
    :create-label
    :create-label-gadget
    :create-list
    :create-main-window
    :create-menu-bar
    :create-menu-shell
    :create-message-box
    :create-message-dialog
    :create-override-shell
    :create-option-menu
    :create-paned-window
    :create-popup-menu
    :create-prompt-dialog
    :create-pulldown-menu
    :create-push-button
    :create-push-button-gadget
    :create-question-dialog
    :create-radio-box
    :create-row-column
    :create-scale
    :create-scroll-bar
    :create-scrolled-list
    :create-scrolled-text
    :create-scrolled-window
    :create-selection-box
    :create-selection-dialog
    :create-separator
    :create-separator-gadget
    :create-text
    :create-text-field
    :create-toggle-button
    :create-toggle-button-gadget
    :create-toplevel-shell
    :create-transient-shell
    :create-warning-dialog
    :create-work-area
    :create-working-dialog
    :create-graph
    :change-grab-cursor
    :create-pixmap-cursor
    :create-font-cursor
    :define-cursor
    :*motif-connection*
    :*x-display*
    :*default-server-host*
    :*default-display-host*
    :*default-display-number*
    :*default-screen-number*
    :*debug-mode*
    :toolkit-connection-stream
    :is-running
    :connection-closed
    :popup
    :popdown
    :manage-popup-child
    :unmanage-popup-child
    :alert-dialog
    :wait-for-input
    :is-mwm-running
    :update-display
    :query-pointer
    :get-screen-size
    :forced-output-mode
    :with-immediate-update-enabled
    :with-immediate-update-disabled
    :last-timestamp-processed
    :add-event-handler
    :remove-event-handler
    :add-graph-relations
    :remove-graph-relations
    :do-layout
    :set-items
    :get-items
    :get-selected-items
    :list-add-item
    :list-add-item-unselected
    :list-delete-item
    :list-delete-pos
    :list-deselect-all-items
    :list-deselect-item
    :list-deselect-pos
    :list-select-item
    :list-select-pos
    :list-set-bottom-item
    :list-set-bottom-pos
    :list-set-horiz-pos
    :list-set-top-item
    :list-set-top-pos
    :toolkit-initialize
    :app-main-loop
    :proceed-from-error
    :run-motif-application
    :run-secondary-process
    :terminate-dispatcher
    :text-insert
    :insert-text
    :text-append
    :append-text
    :text-get-selection
    :text-clear-selection
    :text-cut
    :text-copy
    :text-get-baseline
    :text-get-selection-position
    :text-paste
    :text-pos-to-xy
    :text-remove
    :text-scroll
    :text-set-add-mode
    :text-show-position
    :text-xy-to-pos
    :text-get-last-position
    :text-set-selection
    :text-replace
    :text-get-insertion-position
    :text-set-highlight
    :text-get-substring
    :text-search
    :create-timer
    :destroy-timer
    :change-timer
    :start-timer
    :stop-timer
    :augment-translations
    :override-translations
    :push-translations
    :pop-translations
    :add-tab-group
    :remove-tab-group
    :process-traversal
    :create-application-shell
    :destroy-application
    :create-widget
    :create-unmanaged-widget
    :create-named-widget
    :create-popup-shell
    :destroy-widget
    :realize-widget
    :get-parent
    :make-clx-window
    :raise-widget
    :move-widget
    :manage-widgets
    :unmanage-widgets
    :set-sensitive
    :set-insensitive
    :map-widgets
    :unmap-widgets
    :xt-translate-coordinates
    :set-values
    :get-values
    :get-simple-classes
    :get-composite-classes
    :get-widget-classes
    :get-shell-classes
    :get-popup-classes
    :selection-box-get-child
    :message-box-get-child
    :file-selection-box-get-child
    :command-get-child
    :option-label-gadget
    :option-button-gadget
    :scrolled-window-set-areas
    :main-window-set-areas
    :is-realized
    :get-multi-click-time
    :is-valid-widget-id
    :widget-full-name
    :widget-full-class
    :cascade-button-highlight
    :with-pointer-grab))

;; (in-package :xtk)



;; (export '(
;;     motif-version
;;     ld
;;     add-callback
;;     remove-callback
;;     remove-all-callbacks
;;     has-callbacks
;;     add-protocol-callback
;;     add-wm-protocol-callback
;;     remove-protocol-callback
;;     remove-wm-protocol-callback
;;     center-at
;;     quit-application
;;     popup-exclusive
;;     popup-nonexclusive
;;     popup-none
;;     manage-popup
;;     unmanage-popup
;;     popdown-shell
;;     get-x-colors
;;     create-arrow-button
;;     create-arrow-button-gadget
;;     create-bulletin-board
;;     create-bulletin-board-dialog
;;     create-cascade-button
;;     create-cascade-button-gadget
;;     create-command
;;     create-dialog-shell
;;     create-drawing-area
;;     create-drawn-button
;;     create-error-dialog
;;     create-file-selection-box
;;     create-file-selection-dialog
;;     create-form
;;     create-form-dialog
;;     create-frame
;;     create-information-dialog
;;     create-label
;;     create-label-gadget
;;     create-list
;;     create-main-window
;;     create-menu-bar
;;     create-menu-shell
;;     create-message-box
;;     create-message-dialog
;;     create-override-shell
;;     create-option-menu
;;     create-paned-window
;;     create-popup-menu
;;     create-prompt-dialog
;;     create-pulldown-menu
;;     create-push-button
;;     create-push-button-gadget
;;     create-question-dialog
;;     create-radio-box
;;     create-row-column
;;     create-scale
;;     create-scroll-bar
;;     create-scrolled-list
;;     create-scrolled-text
;;     create-scrolled-window
;;     create-selection-box
;;     create-selection-dialog
;;     create-separator
;;     create-separator-gadget
;;     create-text
;;     create-text-field
;;     create-toggle-button
;;     create-toggle-button-gadget
;;     create-toplevel-shell
;;     create-transient-shell
;;     create-warning-dialog
;;     create-work-area
;;     create-working-dialog
;;     create-graph
;;     change-grab-cursor
;;     create-pixmap-cursor
;;     create-font-cursor
;;     define-cursor
;;     *motif-connection*
;;     *x-display*
;;     *default-server-host*
;;     *default-display-host*
;;     *default-display-number*
;;     *default-screen-number*
;;     *debug-mode*
;;     toolkit-connection-stream
;;     is-running
;;     connection-closed
;;     popup
;;     popdown
;;     manage-popup-child
;;     unmanage-popup-child
;;     alert-dialog
;;     wait-for-input
;;     is-mwm-running
;;     update-display
;;     query-pointer
;;     get-screen-size
;;     forced-output-mode
;;     with-immediate-update-enabled
;;     with-immediate-update-disabled
;;     last-timestamp-processed
;;     add-event-handler
;;     remove-event-handler
;;     add-graph-relations
;;     remove-graph-relations
;;     do-layout
;;     set-items
;;     get-items
;;     get-selected-items
;;     list-add-item
;;     list-add-item-unselected
;;     list-delete-item
;;     list-delete-pos
;;     list-deselect-all-items
;;     list-deselect-item
;;     list-deselect-pos
;;     list-select-item
;;     list-select-pos
;;     list-set-bottom-item
;;     list-set-bottom-pos
;;     list-set-horiz-pos
;;     list-set-top-item
;;     list-set-top-pos
;;     toolkit-initialize
;;     app-main-loop
;;     proceed-from-error
;;     run-motif-application
;;     run-secondary-process
;;     terminate-dispatcher
;;     text-insert
;;     insert-text
;;     text-append
;;     append-text
;;     text-get-selection
;;     text-clear-selection
;;     text-cut
;;     text-copy
;;     text-get-baseline
;;     text-get-selection-position
;;     text-paste
;;     text-pos-to-xy
;;     text-remove
;;     text-scroll
;;     text-set-add-mode
;;     text-show-position
;;     text-xy-to-pos
;;     text-get-last-position
;;     text-set-selection
;;     text-replace
;;     text-get-insertion-position
;;     text-set-highlight
;;     text-get-substring
;;     text-search
;;     create-timer
;;     destroy-timer
;;     change-timer
;;     start-timer
;;     stop-timer
;;     augment-translations
;;     override-translations
;;     push-translations
;;     pop-translations
;;     add-tab-group
;;     remove-tab-group
;;     process-traversal
;;     create-application-shell
;;     destroy-application
;;     create-widget
;;     create-unmanaged-widget
;;     create-named-widget
;;     create-popup-shell
;;     destroy-widget
;;     realize-widget
;;     get-parent
;;     make-clx-window
;;     raise-widget
;;     move-widget
;;     manage-widgets
;;     unmanage-widgets
;;     set-sensitive
;;     set-insensitive
;;     map-widgets
;;     unmap-widgets
;;     xt-translate-coordinates
;;     set-values
;;     get-values
;;     get-simple-classes
;;     get-composite-classes
;;     get-widget-classes
;;     get-shell-classes
;;     get-popup-classes
;;     selection-box-get-child
;;     message-box-get-child
;;     file-selection-box-get-child
;;     command-get-child
;;     option-label-gadget
;;     option-button-gadget
;;     scrolled-window-set-areas
;;     main-window-set-areas
;;     is-realized
;;     get-multi-click-time
;;     is-valid-widget-id
;;     widget-full-name
;;     widget-full-class
;;     cascade-button-highlight
;;     with-pointer-grab))
