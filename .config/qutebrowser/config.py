config.load_autoconfig()

c.aliases = {
    "w": "session-save",
    "q": "close",
    "qa": "quit",
    "qall": "quit",
    "wq": "quit --save",
    "wqa": "quit --save",
}
c.auto_save.interval = 15000
c.auto_save.session = False
c.backend = "webengine"
# c.bindings.key_mappings = {'<Ctrl-[>': '<Escape>', '<Ctrl-6>': '<Ctrl-^>', '<Ctrl-M>': '<Return>', '<Ctrl-J>': '<Return>', '<Shift-Return>': '<Return>', '<Enter>': '<Return>', '<Shift-Enter>': '<Return>', '<Ctrl-Enter>': '<Ctrl-Return>'}

c.colors.completion.category.bg = "black"
c.colors.completion.category.border.bottom = "black"
c.colors.completion.category.border.top = "black"
c.colors.completion.category.fg = "white"
c.colors.completion.even.bg = "black"
c.colors.completion.fg = ["white", "white", "white"]
c.colors.completion.item.selected.bg = "#e8c000"
c.colors.completion.item.selected.border.bottom = "#bbbb00"
c.colors.completion.item.selected.border.top = "#bbbb00"
c.colors.completion.item.selected.fg = "black"
c.colors.completion.item.selected.match.fg = "#ff4444"
c.colors.completion.match.fg = "#ff4444"
c.colors.completion.odd.bg = "black"
c.colors.completion.scrollbar.bg = "#333333"
c.colors.completion.scrollbar.fg = "white"
c.colors.contextmenu.menu.bg = None
c.colors.contextmenu.menu.fg = None
c.colors.contextmenu.selected.bg = None
c.colors.contextmenu.selected.fg = None
c.colors.downloads.bar.bg = "black"
c.colors.downloads.error.bg = "red"
c.colors.downloads.error.fg = "white"
c.colors.downloads.start.bg = "#0000aa"
c.colors.downloads.start.fg = "white"
c.colors.downloads.stop.bg = "#00aa00"
c.colors.downloads.stop.fg = "white"
c.colors.downloads.system.bg = "rgb"
c.colors.downloads.system.fg = "rgb"
c.colors.hints.bg = "qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgba(255, 247, 133, 0.8), stop:1 rgba(255, 197, 66, 0.8))"
c.colors.hints.fg = "black"
c.colors.hints.match.fg = "green"
c.colors.keyhint.bg = "rgba(0, 0, 0, 80%)"
c.colors.keyhint.fg = "#FFFFFF"
c.colors.keyhint.suffix.fg = "#FFFF00"
c.colors.messages.error.bg = "red"
c.colors.messages.error.border = "#bb0000"
c.colors.messages.error.fg = "white"
c.colors.messages.info.bg = "black"
c.colors.messages.info.border = "#333333"
c.colors.messages.info.fg = "white"
c.colors.messages.warning.bg = "darkorange"
c.colors.messages.warning.border = "#d47300"
c.colors.messages.warning.fg = "white"
c.colors.prompts.bg = "black"
c.colors.prompts.border = "0px solid gray"
c.colors.prompts.fg = "white"
c.colors.prompts.selected.bg = "grey"
c.colors.statusbar.caret.bg = "purple"
c.colors.statusbar.caret.fg = "white"
c.colors.statusbar.caret.selection.bg = "#a12dff"
c.colors.statusbar.caret.selection.fg = "white"
c.colors.statusbar.command.bg = "black"
c.colors.statusbar.command.fg = "white"
c.colors.statusbar.command.private.bg = "darkslategray"
c.colors.statusbar.command.private.fg = "white"
c.colors.statusbar.insert.bg = "darkgreen"
c.colors.statusbar.insert.fg = "white"
c.colors.statusbar.normal.bg = "black"
c.colors.statusbar.normal.fg = "white"
c.colors.statusbar.passthrough.bg = "darkblue"
c.colors.statusbar.passthrough.fg = "white"
c.colors.statusbar.private.bg = "#666666"
c.colors.statusbar.private.fg = "white"
c.colors.statusbar.progress.bg = "white"
c.colors.statusbar.url.error.fg = "orange"
c.colors.statusbar.url.fg = "white"
c.colors.statusbar.url.hover.fg = "aqua"
c.colors.statusbar.url.success.http.fg = "orange"
c.colors.statusbar.url.success.https.fg = "lime"
c.colors.statusbar.url.warn.fg = "yellow"
c.colors.tabs.bar.bg = "black"
c.colors.tabs.even.bg = "black"
c.colors.tabs.even.fg = "white"
c.colors.tabs.indicator.error = "#ff0000"
c.colors.tabs.indicator.start = "#0000aa"
c.colors.tabs.indicator.stop = "#00aa00"
c.colors.tabs.indicator.system = "rgb"
c.colors.tabs.odd.bg = "black"
c.colors.tabs.odd.fg = "white"
c.colors.tabs.pinned.even.bg = "darkseagreen"
c.colors.tabs.pinned.even.fg = "white"
c.colors.tabs.pinned.odd.bg = "seagreen"
c.colors.tabs.pinned.odd.fg = "white"
c.colors.tabs.pinned.selected.even.bg = "black"
c.colors.tabs.pinned.selected.even.fg = "white"
c.colors.tabs.pinned.selected.odd.bg = "black"
c.colors.tabs.pinned.selected.odd.fg = "white"
c.colors.tabs.selected.even.bg = "#111111"
c.colors.tabs.selected.even.fg = "white"
c.colors.tabs.selected.odd.bg = "#111111"
c.colors.tabs.selected.odd.fg = "white"
c.colors.webpage.bg = "black"
c.colors.webpage.prefers_color_scheme_dark = True

c.completion.cmd_history_max_items = 100
c.completion.delay = 0
c.completion.height = "1%"
c.completion.min_chars = 1
c.completion.open_categories = ["searchengines", "history"]
c.completion.quick = True
c.completion.scrollbar.padding = 0
c.completion.scrollbar.width = 0
c.completion.show = "always"
c.completion.shrink = True
c.completion.timestamp_format = "%Y-%m-%d"
c.completion.use_best_match = False
c.completion.web_history.exclude = []
c.completion.web_history.max_items = -1

c.confirm_quit = ["multiple-tabs", "downloads"]

c.content.autoplay = False
c.content.cache.appcache = True
c.content.cache.maximum_pages = 0
c.content.cache.size = None
c.content.canvas_reading = True
c.content.cookies.accept = "all"
c.content.cookies.store = True
c.content.default_encoding = "utf-8"
c.content.desktop_capture = "ask"
c.content.dns_prefetch = True
c.content.frame_flattening = False
c.content.fullscreen.window = False
c.content.geolocation = "ask"
c.content.headers.accept_language = "en-US,en;q=0.9"
c.content.headers.custom = {}
c.content.headers.do_not_track = True
c.content.headers.referer = "same-domain"
c.content.headers.user_agent = "Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {qt_key}/{qt_version} {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}"
c.content.host_blocking.enabled = True
c.content.host_blocking.lists = [
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
]
c.content.host_blocking.whitelist = []
c.content.hyperlink_auditing = True
c.content.images = True
c.content.javascript.alert = True
c.content.javascript.can_access_clipboard = False
c.content.javascript.can_close_tabs = False
c.content.javascript.can_open_tabs_automatically = False
c.content.javascript.enabled = True
c.content.javascript.log = {
    "unknown": "debug",
    "info": "debug",
    "warning": "debug",
    "error": "debug",
}
c.content.javascript.modal_dialog = False
c.content.javascript.prompt = True
c.content.local_content_can_access_file_urls = True
c.content.local_content_can_access_remote_urls = False
c.content.local_storage = True
c.content.media_capture = "ask"
c.content.mouse_lock = "ask"
c.content.mute = True
c.content.netrc_file = None
c.content.notifications = "ask"
c.content.pdfjs = True
c.content.persistent_storage = "ask"
c.content.plugins = False
c.content.print_element_backgrounds = True
c.content.private_browsing = False
c.content.proxy = "system"
c.content.proxy_dns_requests = True
c.content.register_protocol_handler = "ask"
c.content.site_specific_quirks = True
c.content.ssl_strict = "ask"
c.content.user_stylesheets = []
c.content.webgl = True
c.content.webrtc_ip_handling_policy = "all-interfaces"
c.content.xss_auditing = False

c.downloads.location.directory = None
c.downloads.location.prompt = True
c.downloads.location.remember = True
c.downloads.location.suggestion = "path"
c.downloads.open_dispatcher = None
c.downloads.position = "top"
c.downloads.remove_finished = -1

c.editor.command = ["emacs", "{file}"]
c.editor.encoding = "utf-8"

# c.fonts.completion.category = 'bold default_size default_family'
# c.fonts.completion.entry = 'default_size default_family'
c.fonts.contextmenu = None
# c.fonts.debug_console = 'default_size default_family'
# c.fonts.default_family = []
# c.fonts.default_size = '10pt'
# c.fonts.downloads = 'default_size default_family'
# c.fonts.hints = 'bold default_size default_family'
# c.fonts.keyhint = 'default_size default_family'
# c.fonts.messages.error = 'default_size default_family'
# c.fonts.messages.info = 'default_size default_family'
# c.fonts.messages.warning = 'default_size default_family'
# c.fonts.prompts = 'default_size sans-serif'
# c.fonts.statusbar = 'default_size default_family'
# c.fonts.tabs = 'default_size default_family'
# c.fonts.web.family.cursive = ''
# c.fonts.web.family.fantasy = ''
# c.fonts.web.family.fixed = ''
# c.fonts.web.family.sans_serif = ''
# c.fonts.web.family.serif = ''
# c.fonts.web.family.standard = ''
# c.fonts.web.size.default = 16
# c.fonts.web.size.default_fixed = 13
# c.fonts.web.size.minimum = 0
# c.fonts.web.size.minimum_logical = 6

c.hints.auto_follow = "unique-match"
c.hints.auto_follow_timeout = 0
c.hints.border = "0px solid #E3BE23"
c.hints.chars = "asdfghjkl"
c.hints.dictionary = "/usr/share/dict/words"
c.hints.find_implementation = "python"
c.hints.hide_unmatched_rapid_hints = True
c.hints.leave_on_load = True
c.hints.min_chars = 1
c.hints.mode = "letter"
c.hints.next_regexes = [
    "\\bnext\\b",
    "\\bmore\\b",
    "\\bnewer\\b",
    "\\b[>→≫]\\b",
    "\\b(>>|»)\\b",
    "\\bcontinue\\b",
]
c.hints.prev_regexes = [
    "\\bprev(ious)?\\b",
    "\\bback\\b",
    "\\bolder\\b",
    "\\b[<←≪]\\b",
    "\\b(<<|«)\\b",
]
c.hints.scatter = True
c.hints.selectors = {
    "all": [
        "a",
        "area",
        "textarea",
        "select",
        'input:not([type="hidden"])',
        "button",
        "frame",
        "iframe",
        "img",
        "link",
        "summary",
        "[onclick]",
        "[onmousedown]",
        '[role="link"]',
        '[role="option"]',
        '[role="button"]',
        "[ng-click]",
        "[ngClick]",
        "[data-ng-click]",
        "[x-ng-click]",
        "[tabindex]",
    ],
    "links": ["a[href]", "area[href]", "link[href]", '[role="link"][href]'],
    "images": ["img"],
    "media": ["audio", "img", "video"],
    "url": ["[src]", "[href]"],
    "inputs": [
        'input[type="text"]',
        'input[type="date"]',
        'input[type="datetime-local"]',
        'input[type="email"]',
        'input[type="month"]',
        'input[type="number"]',
        'input[type="password"]',
        'input[type="search"]',
        'input[type="tel"]',
        'input[type="time"]',
        'input[type="url"]',
        'input[type="week"]',
        "input:not([type])",
        "textarea",
    ],
}
c.hints.uppercase = True

c.history_gap_interval = 30

c.input.escape_quits_reporter = True
c.input.forward_unbound_keys = "auto"
c.input.insert_mode.auto_enter = True
c.input.insert_mode.auto_leave = True
c.input.insert_mode.auto_load = False
c.input.insert_mode.leave_on_load = True
c.input.insert_mode.plugins = False
c.input.links_included_in_focus_chain = True
c.input.partial_timeout = 5000
c.input.rocker_gestures = False
# c.input.spatial_navigation = False

c.keyhint.blacklist = []
c.keyhint.delay = 500
c.keyhint.radius = 6

c.messages.timeout = 2000

c.new_instance_open_target = "tab"
c.new_instance_open_target_window = "last-focused"

c.prompt.filebrowser = True
c.prompt.radius = 0

c.qt.args = []
c.qt.force_platform = None
c.qt.force_platformtheme = None
c.qt.force_software_rendering = "none"
c.qt.highdpi = False
c.qt.low_end_device_mode = "auto"
c.qt.process_model = "process-per-site-instance"

c.scrolling.bar = "when-searching"
c.scrolling.smooth = False

c.search.ignore_case = "smart"
c.search.incremental = True

c.session.default_name = None
c.session.lazy_restore = False

c.spellcheck.languages = []

c.statusbar.hide = False
c.statusbar.padding = {"top": 1, "bottom": 1, "left": 0, "right": 0}
c.statusbar.position = "bottom"
c.statusbar.widgets = [
    "keypress",
    "url",
    "scroll",
    "history",
    "tabs",
    "keypress",
    "progress",
]

c.tabs.background = False
c.tabs.close_mouse_button = "middle"
c.tabs.close_mouse_button_on_bar = "new-tab"
c.tabs.favicons.scale = 1.0
c.tabs.favicons.show = "pinned"
c.tabs.focus_stack_size = 10
c.tabs.indicator.padding = {"top": 2, "bottom": 2, "left": 0, "right": 4}
c.tabs.indicator.width = 3
c.tabs.last_close = "ignore"
c.tabs.max_width = -1
c.tabs.min_width = -1
c.tabs.mode_on_change = "normal"
c.tabs.mousewheel_switching = True
c.tabs.new_position.related = "next"
c.tabs.new_position.stacking = True
c.tabs.new_position.unrelated = "last"
c.tabs.padding = {"top": 0, "bottom": 0, "left": 5, "right": 5}
c.tabs.pinned.frozen = True
c.tabs.pinned.shrink = True
c.tabs.position = "top"
c.tabs.select_on_remove = "next"
c.tabs.show = "always"
c.tabs.show_switching_delay = 800
c.tabs.tabs_are_windows = False
c.tabs.title.alignment = "left"
c.tabs.title.format = "{audio}{index}: {current_title}"
c.tabs.title.format_pinned = "{index}"
c.tabs.tooltips = True
c.tabs.undo_stack_size = 100
c.tabs.width = "20%"
c.tabs.wrap = True

c.url.auto_search = "naive"
c.url.default_page = "about:blank"
c.url.incdec_segments = ["path", "query"]
c.url.open_base_url = False
c.url.searchengines = {"DEFAULT": "https://google.com/search?q={}"}
c.url.start_pages = ["about:blank"]
c.url.yank_ignored_parameters = [
    "ref",
    "utm_source",
    "utm_medium",
    "utm_campaign",
    "utm_term",
    "utm_content",
]

c.window.hide_decoration = True
c.window.title_format = "{perc}{current_title}{title_sep}"

c.zoom.default = "100%"
c.zoom.levels = [
    "25%",
    "33%",
    "50%",
    "67%",
    "75%",
    "90%",
    "100%",
    "110%",
    "125%",
    "150%",
    "175%",
    "200%",
    "250%",
    "300%",
    "400%",
    "500%",
]
c.zoom.mouse_divider = 512
c.zoom.text_only = False
