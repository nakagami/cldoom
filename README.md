# CL-DOOM for ExLisp

**ExLisp** (running on Elixir / BEAM) 上で動作する、ターミナルベースの 3D レイキャスティング FPS (DOOM) です。

## 特徴

- **3D レイキャスティング レンダリングエンジン**: DDA (Digital Differential Analyzer) アルゴリズムによる高速な疑似3D壁・床・天井・ライティング描画
- **ビルボードスプライト**: 敵モンスター (Imp, Zombie, Cacodemon)、ドラム缶、アイテム (Medkit, Armor, Ammo, Shotgun, BFG)
- **戦闘・武器システム**:
  - `[1]` Fist (拳)
  - `[2]` Pistol (ハンドガン)
  - `[3]` Shotgun (ショットガン / マズルフラッシュ描画付き)
  - `[4]` BFG 9000 (強力なエネルギー兵器)
- **敵 AI**: 視界・距離による索敵、接近、攻撃
- **爆発性ドラム缶**: 銃撃で誘爆し、周囲の敵・プレイヤーに範囲ダメージ
- **DOOM ステータスバー (HUD)**:
  - ヘルス (HP)、アーマー (ARM)、状況に応じて表情が変わる DOOMGUY フェイス (`^_^`, `Ò_Ó`, `>_<`, `•д•`, `x_x`)、残弾数、装備武器、撃破数
- **オートマップ (Minimap)**: 探索済みエリアと敵・アイテムのリアルタイムレーダー表示
- **全3エピソード / マップ**:
  - E1M1: HANGAR
  - E1M2: NUCLEAR PLANT
  - E1M8: PHOBOS ANOMALY (ボスステージ)

## 動作要件

- Elixir / Erlang
- ExLisp (`mix escript.install hex exlisp` または `~/.mix/escripts/exlisp`)

## 起動方法

```bash
./doom
```

## 操作方法

| キー | アクション |
| :--- | :--- |
| `W` / `S` / `↑` / `↓` | 前進 / 後退 |
| `A` / `D` | 左平行移動 / 右平行移動 (Strafe) |
| `Q` / `E` / `←` / `→` | 視点左回転 / 視点右回転 |
| `Space` / `F` | 武器発射 (Fire) |
| `1` / `2` / `3` / `4` | 武器切り替え (Fist / Pistol / Shotgun / BFG) |
| `M` | オートマップ (Minimap) の表示切替 |
| `X` | ゲーム終了 (Quit) |
