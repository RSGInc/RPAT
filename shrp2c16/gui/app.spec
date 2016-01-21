# -*- mode: python -*-
a = Analysis(['app.py'],
             pathex=['C:\\development\\shrp2c16\\branches\\ui-upgrade\\shrp2c16\\gui'],
             hiddenimports=[],
             hookspath=None,
             runtime_hooks=None)
pyz = PYZ(a.pure)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=True,
          name='RPAT.exe',
          debug=True,
          strip=None,
          upx=True,
          console=True,
          icon='C:\\development\\shrp2c16\\branches\\ui-upgrade\\shrp2c16\\gui\\views\\img\\RPAT.ico')
coll = COLLECT(exe,
               a.binaries,
               a.zipfiles,
               a.datas,
               strip=None,
               upx=True,
               name='app')
